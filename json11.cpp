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

#include "json11.hpp"
#include <charconv>
#include <cmath>
#include <limits>

namespace json11 {

static constexpr int max_depth = 200;

namespace {

[[nodiscard]] 
std::string esc(char c) {
    if (static_cast<uint8_t>(c) >= 0x20 && static_cast<uint8_t>(c) <= 0x7f) {
        return std::string("'") + c + "' (" + std::to_string(static_cast<int>(c)) + ")";
    }
    return "(" + std::to_string(static_cast<int>(c)) + ")";
}

[[nodiscard]] 
constexpr bool in_range(long x, long lower, long upper) noexcept {
    return (x >= lower && x <= upper);
}

class JsonParser final {
public:
    JsonParser(std::string_view str, std::string& err, JsonParse strategy) 
        : m_str(str), m_index(0), m_err(err), m_failed(false), m_strategy(strategy) {}

    template<typename T>
    T fail(std::string&& msg, T err_ret = T()) {
        if (!m_failed)
            m_err = std::move(msg);
        m_failed = true;
        return err_ret;
    }

    void consume_whitespace() {
        while (m_index < m_str.size() && std::isspace(m_str[m_index])) {
            ++m_index;
        }
    }

    bool consume_comment() {
        if (m_index >= m_str.size() || m_str[m_index] != '/') {
            return false;
        }

        ++m_index;
        if (m_index == m_str.size()) {
            return fail("unexpected end of input after start of comment", false);
        }

        if (m_str[m_index] == '/') {
            ++m_index;
            while (m_index < m_str.size() && m_str[m_index] != '\n') {
                ++m_index;
            }
            return true;
        }

        if (m_str[m_index] == '*') {
            ++m_index;
            while (m_index < m_str.size() - 1) {
                if (m_str[m_index] == '*' && m_str[m_index + 1] == '/') {
                    m_index += 2;
                    return true;
                }
                ++m_index;
            }
            return fail("unexpected end of input inside multi-line comment", false);
        }

        return fail("malformed comment", false);
    }

    void consume_garbage() {
        consume_whitespace();
        if (m_strategy == JsonParse::COMMENTS) {
            bool comment_found = false;
            do {
                comment_found = consume_comment();
                if (m_failed) return;
                consume_whitespace();
            } while(comment_found);
        }
    }

    char get_next_token() {
        consume_garbage();
        if (m_failed) return static_cast<char>(0);
        if (m_index == m_str.size())
            return fail("unexpected end of input", static_cast<char>(0));

        return m_str[m_index++];
    }

    [[nodiscard]] bool failed() const noexcept { return m_failed; }
    [[nodiscard]] size_t index() const noexcept { return m_index; }

private:
    const std::string_view m_str;
    size_t m_index;
    std::string& m_err;
    bool m_failed;
    const JsonParse m_strategy;

    void encode_utf8(long pt, std::string& out) {
        if (pt < 0) return;

        if (pt < 0x80) {
            out += static_cast<char>(pt);
        } else if (pt < 0x800) {
            out += static_cast<char>((pt >> 6) | 0xC0);
            out += static_cast<char>((pt & 0x3F) | 0x80);
        } else if (pt < 0x10000) {
            out += static_cast<char>((pt >> 12) | 0xE0);
            out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
            out += static_cast<char>((pt & 0x3F) | 0x80);
        } else {
            out += static_cast<char>((pt >> 18) | 0xF0);
            out += static_cast<char>(((pt >> 12) & 0x3F) | 0x80);
            out += static_cast<char>(((pt >> 6) & 0x3F) | 0x80);
            out += static_cast<char>((pt & 0x3F) | 0x80);
        }
    }

    [[nodiscard]] 
    std::string parse_string() {
        std::string out;
        long last_escaped_codepoint = -1;
        
        while (true) {
            if (m_index == m_str.size())
                return fail("unexpected end of input in string", "");

            char ch = m_str[m_index++];

            if (ch == '"') {
                encode_utf8(last_escaped_codepoint, out);
                return out;
            }

            if (in_range(ch, 0, 0x1f))
                return fail("unescaped " + esc(ch) + " in string", "");

            if (ch != '\\') {
                encode_utf8(last_escaped_codepoint, out);
                last_escaped_codepoint = -1;
                out += ch;
                continue;
            }

            // Handle escapes
            if (m_index == m_str.size())
                return fail("unexpected end of input in string", "");

            ch = m_str[m_index++];
            if (ch == 'u') {
                // Parse 4-byte escape sequence
                if (m_index + 4 > m_str.size()) {
                    return fail("bad \\u escape", "");
                }

                std::string_view esc = m_str.substr(m_index, 4);
                for (char c : esc) {
                    if (!std::isxdigit(c))
                        return fail("bad \\u escape: " + std::string(esc), "");
                }

                long codepoint = std::strtol(std::string(esc).c_str(), nullptr, 16);

                if (in_range(last_escaped_codepoint, 0xD800, 0xDBFF) &&
                    in_range(codepoint, 0xDC00, 0xDFFF)) {
                    encode_utf8((((last_escaped_codepoint - 0xD800) << 10) |
                               (codepoint - 0xDC00)) + 0x10000, out);
                    last_escaped_codepoint = -1;
                } else {
                    encode_utf8(last_escaped_codepoint, out);
                    last_escaped_codepoint = codepoint;
                }

                m_index += 4;
                continue;
            }

            encode_utf8(last_escaped_codepoint, out);
            last_escaped_codepoint = -1;

            switch(ch) {
                case 'b': out += '\b'; break;
                case 'f': out += '\f'; break;
                case 'n': out += '\n'; break;
                case 'r': out += '\r'; break;
                case 't': out += '\t'; break;
                case '"': case '\\': case '/': out += ch; break;
                default: return fail("invalid escape character " + esc(ch), "");
            }
        }
    }

    [[nodiscard]] 
    Json parse_number() {
        size_t start_pos = m_index;

        if (m_str[m_index] == '-')
            ++m_index;

        // Integer part
        if (m_str[m_index] == '0') {
            ++m_index;
            if (in_range(m_str[m_index], '0', '9'))
                return fail("leading 0s not permitted in numbers");
        } else if (in_range(m_str[m_index], '1', '9')) {
            ++m_index;
            while (m_index < m_str.size() && in_range(m_str[m_index], '0', '9'))
                ++m_index;
        } else {
            return fail("invalid " + esc(m_str[m_index]) + " in number");
        }

        // Try parsing as integer
        if (m_str[m_index] != '.' && m_str[m_index] != 'e' && m_str[m_index] != 'E'
            && (m_index - start_pos) <= static_cast<size_t>(std::numeric_limits<int>::digits10)) {
            int value;
            const auto [ptr, ec] = std::from_chars(m_str.data() + start_pos, 
                                                 m_str.data() + m_index, value);
            if (ec == std::errc()) {
                return Json(value);
            }
        }

        // Parse as double
        if (m_index < m_str.size() && m_str[m_index] == '.') {
            ++m_index;
            if (!in_range(m_str[m_index], '0', '9'))
                return fail("at least one digit required in fractional part");

            while (m_index < m_str.size() && in_range(m_str[m_index], '0', '9'))
                ++m_index;
        }

        if (m_index < m_str.size() && (m_str[m_index] == 'e' || m_str[m_index] == 'E')) {
            ++m_index;

            if (m_index < m_str.size() && (m_str[m_index] == '+' || m_str[m_index] == '-'))
                ++m_index;

            if (!in_range(m_str[m_index], '0', '9'))
                return fail("at least one digit required in exponent");

            while (m_index < m_str.size() && in_range(m_str[m_index], '0', '9'))
                ++m_index;
        }

        double value;
        const auto [ptr, ec] = std::from_chars(m_str.data() + start_pos, 
                                             m_str.data() + m_index, value);
        if (ec == std::errc()) {
            return Json(value);
        }
        
        return fail("failed to parse number");
    }

    [[nodiscard]]
    Json expect(std::string_view expected, Json res) {
        --m_index;
        if (m_str.substr(m_index, expected.length()) == expected) {
            m_index += expected.length();
            return res;
        }
        return fail("parse error: expected " + std::string(expected) + ", got " + 
                   std::string(m_str.substr(m_index, expected.length())));
    }

    [[nodiscard]] 
    Json parse_json(int depth) {
        if (depth > max_depth) {
            return fail("exceeded maximum nesting depth");
        }

        char ch = get_next_token();
        if (m_failed) return Json();

        if (ch == '-' || (ch >= '0' && ch <= '9')) {
            --m_index;
            return parse_number();
        }

        switch(ch) {
            case 't': return expect("true", true);
            case 'f': return expect("false", false);
            case 'n': return expect("null", Json());
            case '"': return Json(parse_string());
            case '{': return parse_object(depth);
            case '[': return parse_array(depth);
            default: return fail("expected value, got " + esc(ch));
        }
    }

    [[nodiscard]] 
    Json parse_object(int depth) {
        Json::object data;
        
        char ch = get_next_token();
        if (ch == '}') return data;

        while (true) {
            if (ch != '"')
                return fail("expected '\"' in object, got " + esc(ch));

            std::string key = parse_string();
            if (m_failed) return Json();

            ch = get_next_token();
            if (ch != ':')
                return fail("expected ':' in object, got " + esc(ch));

            data.emplace(std::move(key), parse_json(depth + 1));
            if (m_failed) return Json();

            ch = get_next_token();
            if (ch == '}') break;
            if (ch != ',')
                return fail("expected ',' in object, got " + esc(ch));

            ch = get_next_token();
        }
        return data;
    }

    [[nodiscard]] 
    Json parse_array(int depth) {
        Json::array data;
        
        char ch = get_next_token();
        if (ch == ']') return data;

        while (true) {
            --m_index;
            data.push_back(parse_json(depth + 1));
            if (m_failed) return Json();

            ch = get_next_token();
            if (ch == ']') break;
            if (ch != ',')
                return fail("expected ',' in array, got " + esc(ch));

            ch = get_next_token();
        }
        return data;
    }
}; // class JsonParser

} // anonymous namespace

// JSON value serialization
void Json::dump(std::string& out) const {
    std::visit([&out](const auto& value) {
        using T = std::decay_t<decltype(value)>;
        if constexpr (std::is_same_v<T, NullType>) {
            out += "null";
        } else if constexpr (std::is_same_v<T, double>) {
            if (std::isfinite(value)) {
                char buf[32];
                snprintf(buf, sizeof buf, "%.17g", value);
                out += buf;
            } else {
                out += "null";
            }
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
                        if (static_cast<uint8_t>(c) < 0x20) {
                            char buf[7];
                            snprintf(buf, sizeof buf, "\\u%04x", c);
                            out += buf;
                        } else {
                            out += c;
                        }
                }
            }
            out += '"';
        } else if constexpr (std::is_same_v<T, Json::array>) {
            out += '[';
            bool first = true;
            for (const auto& item : value) {
                if (!first) out += ',';
                item.dump(out);
                first = false;
            }
            out += ']';
        } else if constexpr (std::is_same_v<T, Json::object>) {
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

// Parsing functions
Json Json::parse(std::string_view in, std::string& err, JsonParse strategy) {
    JsonParser parser{in, err, strategy};
    Json result = parser.parse_json(0);

    parser.consume_garbage();
    if (parser.failed())
        return Json();
    if (parser.index() != in.size())
        return parser.fail("unexpected trailing " + esc(in[parser.index()]));

    return result;
}

std::vector<Json> Json::parse_multi(
    std::string_view in,
    std::string::size_type& parser_stop_pos,
    std::string& err,
    JsonParse strategy) {
    JsonParser parser{in, err, strategy};
    parser_stop_pos = 0;
    std::vector<Json> json_vec;
    
    while (parser.index() != in.size() && !parser.failed()) {
        json_vec.push_back(parser.parse_json(0));
        if (parser.failed())
            break;

        parser.consume_garbage();
        if (parser.failed())
            break;
        parser_stop_pos = parser.index();
    }
    return json_vec;
}

bool Json::has_shape(const shape& types, std::string& err) const {
    if (!std::holds_alternative<object>(m_value)) {
        err = "expected JSON object, got " + std::string(type_name());
        return false;
    }

    const auto& obj_items = std::get<object>(m_value);
    for (const auto& [key, expected_type] : types) {
        const auto it = obj_items.find(key);
        if (it == obj_items.end()) {
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

} // namespace json11