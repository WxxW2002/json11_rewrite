/* json11
 *
 * json11 is a tiny JSON library for C++11, providing JSON parsing and serialization.
 *
 * The core object provided by the library is json11::Json. A Json object represents any JSON
 * value: null, bool, number (int or double), string (std::string), array (std::vector), or
 * object (std::map).
 *
 * Json objects act like values: they can be assigned, copied, moved, compared for equality or
 * order, etc. There are also helper methods Json::dump, to serialize a Json to a string, and
 * Json::parse (static) to parse a std::string as a Json object.
 *
 * Internally, the various types of Json object are represented by the JsonValue class
 * hierarchy.
 *
 * A note on numbers - JSON specifies the syntax of number formatting but not its semantics,
 * so some JSON implementations distinguish between integers and floating-point numbers, while
 * some don't. In json11, we choose the latter. Because some JSON implementations (namely
 * Javascript itself) treat all numbers as the same type, distinguishing the two leads
 * to JSON that will be *silently* changed by a round-trip through those implementations.
 * Dangerous! To avoid that risk, json11 stores all numbers as double internally, but also
 * provides integer helpers.
 *
 * Fortunately, double-precision IEEE754 ('double') can precisely store any integer in the
 * range +/-2^53, which includes every 'int' on most systems. (Timestamps often use int64
 * or long long to avoid the Y2038K problem; a double storing microseconds since some epoch
 * will be exact for +/- 275 years.)
 */

/* Copyright (c) 2013 Dropbox, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#pragma once

#include <string>
#include <string_view>
#include <vector>
#include <map>
#include <variant>
#include <memory>
#include <compare>
#include <initializer_list>

namespace json11 {

enum class JsonParse {
    STANDARD, 
    COMMENTS
};

class Json final {
public:
    enum class Type {
        NUL, NUMBER, BOOL, STRING, ARRAY, OBJECT
    };

    using array = std::vector<Json>;
    using object = std::map<std::string, Json>;
    
    struct NullType {};
    
    using ValueType = std::variant<
        NullType,
        double,
        bool,
        std::string,
        array,
        object
    >;

    // Constructors
    constexpr Json() noexcept : m_value(NullType{}) {}
    constexpr Json(std::nullptr_t) noexcept : m_value(NullType{}) {}
    Json(double value) : m_value(value) {}
    Json(int value) : m_value(static_cast<double>(value)) {}
    Json(bool value) : m_value(value) {}
    Json(std::string_view value) : m_value(std::string(value)) {}
    Json(const char* value) : m_value(std::string(value)) {}
    Json(const array& values) : m_value(values) {}
    Json(array&& values) noexcept : m_value(std::move(values)) {}
    Json(const object& values) : m_value(values) {}
    Json(object&& values) noexcept : m_value(std::move(values)) {}

    // Implicit constructors
    template <typename T>
    Json(const T& t) : Json(t.to_json()) {}

    template <typename M>
    Json(const M& m,
         std::enable_if_t<
             std::is_constructible_v<std::string, decltype(std::declval<M>().begin()->first)> &&
             std::is_constructible_v<Json, decltype(std::declval<M>().begin()->second)>,
         int> = 0)
        : Json(object(m.begin(), m.end())) {}

    template <typename V>
    Json(const V& v,
         std::enable_if_t<
             std::is_constructible_v<Json, decltype(*std::declval<V>().begin())>,
         int> = 0)
        : Json(array(v.begin(), v.end())) {}

    Json(void*) = delete;

    // Type checks
    [[nodiscard]] Type type() const noexcept {
        return std::visit([](const auto& value) -> Type {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<T, NullType>) return Type::NUL;
            else if constexpr (std::is_same_v<T, double>) return Type::NUMBER;
            else if constexpr (std::is_same_v<T, bool>) return Type::BOOL;
            else if constexpr (std::is_same_v<T, std::string>) return Type::STRING;
            else if constexpr (std::is_same_v<T, array>) return Type::ARRAY;
            else if constexpr (std::is_same_v<T, object>) return Type::OBJECT;
        }, m_value);
    }

    [[nodiscard]] constexpr bool is_null() const noexcept { return type() == Type::NUL; }
    [[nodiscard]] constexpr bool is_number() const noexcept { return type() == Type::NUMBER; }
    [[nodiscard]] constexpr bool is_bool() const noexcept { return type() == Type::BOOL; }
    [[nodiscard]] constexpr bool is_string() const noexcept { return type() == Type::STRING; }
    [[nodiscard]] constexpr bool is_array() const noexcept { return type() == Type::ARRAY; }
    [[nodiscard]] constexpr bool is_object() const noexcept { return type() == Type::OBJECT; }

    // Value accessors
    [[nodiscard]] double number_value() const {
        return std::holds_alternative<double>(m_value) ? 
               std::get<double>(m_value) : 0.0;
    }
    
    [[nodiscard]] int int_value() const {
        return static_cast<int>(number_value());
    }
    
    [[nodiscard]] bool bool_value() const {
        return std::holds_alternative<bool>(m_value) ? 
               std::get<bool>(m_value) : false;
    }
    
    [[nodiscard]] const std::string& string_value() const {
        static const std::string empty;
        return std::holds_alternative<std::string>(m_value) ? 
               std::get<std::string>(m_value) : empty;
    }
    
    [[nodiscard]] const array& array_items() const {
        static const array empty;
        return std::holds_alternative<array>(m_value) ? 
               std::get<array>(m_value) : empty;
    }
    
    [[nodiscard]] const object& object_items() const {
        static const object empty;
        return std::holds_alternative<object>(m_value) ? 
               std::get<object>(m_value) : empty;
    }

    // Array & object accessors
    [[nodiscard]] const Json& operator[](size_t i) const {
        static const Json null;
        if (is_array() && i < array_items().size()) {
            return array_items()[i];
        }
        return null;
    }

    [[nodiscard]] const Json& operator[](std::string_view key) const {
        static const Json null;
        if (is_object()) {
            const auto& obj = object_items();
            auto it = obj.find(std::string(key));
            if (it != obj.end()) {
                return it->second;
            }
        }
        return null;
    }

    // Serialization
    void dump(std::string& out) const {
        std::visit([&out](const auto& value) {
            using T = std::decay_t<decltype(value)>;
            if constexpr (std::is_same_v<T, NullType>) {
                out += "null";
            } else if constexpr (std::is_same_v<T, double>) {
                char buf[32];
                snprintf(buf, sizeof buf, "%.17g", value);
                out += buf;
            } else if constexpr (std::is_same_v<T, bool>) {
                out += value ? "true" : "false";
            } else if constexpr (std::is_same_v<T, std::string>) {
                out += '"';
                for (char c : value) {
                    switch (c) {
                        case '"': out += "\\\""; break;
                        case '\\': out += "\\\\"; break;
                        case '\b': out += "\\b"; break;
                        case '\f': out += "\\f"; break;
                        case '\n': out += "\\n"; break;
                        case '\r': out += "\\r"; break;
                        case '\t': out += "\\t"; break;
                        default:
                            if (static_cast<unsigned char>(c) < 0x20) {
                                char buf[7];
                                snprintf(buf, sizeof buf, "\\u%04x", c);
                                out += buf;
                            } else {
                                out += c;
                            }
                    }
                }
                out += '"';
            } else if constexpr (std::is_same_v<T, array>) {
                out += '[';
                bool first = true;
                for (const auto& item : value) {
                    if (!first) out += ',';
                    item.dump(out);
                    first = false;
                }
                out += ']';
            } else if constexpr (std::is_same_v<T, object>) {
                out += '{';
                bool first = true;
                for (const auto& [k, v] : value) {
                    if (!first) out += ',';
                    Json(k).dump(out);
                    out += ':';
                    v.dump(out);
                    first = false;
                }
                out += '}';
            }
        }, m_value);
    }

    [[nodiscard]] std::string dump() const {
        std::string out;
        dump(out);
        return out;
    }

    // Parsing
    [[nodiscard]] static Json parse(std::string_view in, 
                                  std::string& err,
                                  JsonParse strategy = JsonParse::STANDARD);

    [[nodiscard]] static Json parse(const char* in,
                                  std::string& err,
                                  JsonParse strategy = JsonParse::STANDARD) {
        return in ? parse(std::string_view(in), err, strategy) 
                 : (err = "null input", Json(nullptr));
    }

    [[nodiscard]] static std::vector<Json> parse_multi(
        std::string_view in,
        std::string::size_type& parser_stop_pos,
        std::string& err,
        JsonParse strategy = JsonParse::STANDARD);

    [[nodiscard]] static std::vector<Json> parse_multi(
        std::string_view in,
        std::string& err,
        JsonParse strategy = JsonParse::STANDARD) {
        std::string::size_type parser_stop_pos;
        return parse_multi(in, parser_stop_pos, err, strategy);
    }

    // Comparison operators
    bool operator==(const Json& rhs) const {
        return m_value == rhs.m_value;
    }

    auto operator<=>(const Json& rhs) const {
        return m_value <=> rhs.m_value;
    }

    // Shape validation
    using shape = std::initializer_list<std::pair<std::string, Type>>;
    [[nodiscard]] bool has_shape(const shape& types, std::string& err) const {
        if (!is_object()) {
            err = "expected JSON object, got " + std::string(type_name());
            return false;
        }

        for (const auto& [key, expected_type] : types) {
            const auto it = object_items().find(key);
            if (it == object_items().end()) {
                err = "missing field \"" + key + "\"";
                return false;
            }
            if (it->second.type() != expected_type) {
                err = "field \"" + key + "\" has wrong type";
                return false;
            }
        }
        return true;
    }

private:
    [[nodiscard]] const char* type_name() const noexcept {
        switch (type()) {
            case Type::NUL: return "null";
            case Type::NUMBER: return "number";
            case Type::BOOL: return "bool";
            case Type::STRING: return "string";
            case Type::ARRAY: return "array";
            case Type::OBJECT: return "object";
        }
        return "unknown";
    }

    ValueType m_value;
};

} // namespace json11