/*
 * Define JSON11_TEST_CUSTOM_CONFIG to 1 if you want to build this tester into
 * your own unit-test framework rather than a stand-alone program.  By setting
 * The values of the variables included below, you can insert your own custom
 * code into this file as it builds, in order to make it into a test case for
 * your favorite framework.
 */
#if !JSON11_TEST_CUSTOM_CONFIG
#define JSON11_TEST_CPP_PREFIX_CODE
#define JSON11_TEST_CPP_SUFFIX_CODE
#define JSON11_TEST_STANDALONE_MAIN 1
#define JSON11_TEST_CASE(name) static void name()
#define JSON11_TEST_ASSERT(b) assert(b)
#ifdef NDEBUG
#undef NDEBUG
#endif
#endif

/*
 * Enable or disable code which demonstrates the behavior change in Xcode 7 / Clang 3.7,
 * introduced by DR1467 and described here: https://github.com/dropbox/json11/issues/86
 * Defaults to off since it doesn't appear the standards committee is likely to act
 * on this, so it needs to be considered normal behavior.
 */
#ifndef JSON11_ENABLE_DR1467_CANARY
#define JSON11_ENABLE_DR1467_CANARY 0
#endif

/*
 * Beginning of standard source file, which makes use of the customizations above.
 */
#include <cassert>
#include <string>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <sstream>
#include <list>
#include <set>
#include <unordered_map>
#include <algorithm>
#include <type_traits>
#include "json11.hpp"

JSON11_TEST_CPP_PREFIX_CODE

using namespace json11;
using std::string;

// Type trait checks
static_assert(std::is_nothrow_constructible_v<Json>);
static_assert(std::is_nothrow_default_constructible_v<Json>);
static_assert(std::is_copy_constructible_v<Json>);
static_assert(std::is_nothrow_move_constructible_v<Json>);
static_assert(std::is_copy_assignable_v<Json>);
static_assert(std::is_nothrow_move_assignable_v<Json>);
static_assert(std::is_nothrow_destructible_v<Json>);

JSON11_TEST_CASE(json11_test) {
    const string simple_test =
        R"({"k1":"v1", "k2":42, "k3":["a",123,true,false,null]})";

    string err;
    const auto json = Json::parse(simple_test, err);

    std::cout << "k1: " << json["k1"].string_value() << "\n";
    std::cout << "k3: " << json["k3"].dump() << "\n";

    for (const auto& k : json["k3"].array_items()) {
        std::cout << "    - " << k.dump() << "\n";
    }

    string comment_test = R"({
      // comment /* with nested comment */
      "a": 1,
      // comment
      // continued
      "b": "text",
      /* multi
         line
         comment
        // line-comment-inside-multiline-comment
      */
      // and single-line comment
      // and single-line comment /* multiline inside single line */
      "c": [1, 2, 3]
      // and single-line comment at end of object
    })";

    string err_comment;
    auto json_comment = Json::parse(comment_test, err_comment, JsonParse::COMMENTS);
    JSON11_TEST_ASSERT(!json_comment.is_null());
    JSON11_TEST_ASSERT(err_comment.empty());

    // Test trailing comments
    comment_test = R"({"a": 1}//trailing line comment)";
    json_comment = Json::parse(comment_test, err_comment, JsonParse::COMMENTS);
    JSON11_TEST_ASSERT(!json_comment.is_null());
    JSON11_TEST_ASSERT(err_comment.empty());

    comment_test = R"({"a": 1}/*trailing multi-line comment*/)";
    json_comment = Json::parse(comment_test, err_comment, JsonParse::COMMENTS);
    JSON11_TEST_ASSERT(!json_comment.is_null());
    JSON11_TEST_ASSERT(err_comment.empty());

    // Test failing comments
    const std::vector<string> failing_comments = {
        "{\n/* unterminated comment\n\"a\": 1,\n}",
        "{\n/* unterminated trailing comment }",
        "{\n/ / bad comment }",
        "{// bad comment }",
        "{\n\"a\": 1\n}/",
        "{/* bad\ncomment *}"
    };

    for (const auto& test : failing_comments) {
        json_comment = Json::parse(test, err_comment, JsonParse::COMMENTS);
        JSON11_TEST_ASSERT(json_comment.is_null());
        JSON11_TEST_ASSERT(!err_comment.empty());
        err_comment.clear();
    }

    // Container type compatibility tests
    std::list<int> l1{1, 2, 3};
    std::vector<int> l2{1, 2, 3};
    std::set<int> l3{1, 2, 3};
    JSON11_TEST_ASSERT(Json(l1) == Json(l2));
    JSON11_TEST_ASSERT(Json(l2) == Json(l3));

    std::map<string, string> m1{{"k1", "v1"}, {"k2", "v2"}};
    std::unordered_map<string, string> m2{{"k1", "v1"}, {"k2", "v2"}};
    JSON11_TEST_ASSERT(Json(m1) == Json(m2));

    // JSON literal construction
    const Json obj = Json::object{
        {"k1", "v1"},
        {"k2", 42.0},
        {"k3", Json::array{"a", 123.0, true, false, nullptr}}
    };

    std::cout << "obj: " << obj.dump() << "\n";
    JSON11_TEST_ASSERT(obj.dump() == R"({"k1": "v1", "k2": 42, "k3": ["a", 123, true, false, null]})");

    // Type coercion tests
    JSON11_TEST_ASSERT(Json("a").number_value() == 0);
    JSON11_TEST_ASSERT(Json("a").string_value() == "a");
    JSON11_TEST_ASSERT(Json().number_value() == 0);

    JSON11_TEST_ASSERT(obj == json);
    JSON11_TEST_ASSERT(Json(42) == Json(42.0));
    JSON11_TEST_ASSERT(Json(42) != Json(42.1));

    // Unicode escape sequence tests
    const string unicode_escape_test =
        R"(["blah\ud83d\udca9blah\ud83dblah\udca9blah\u0000blah\u1234"])";

    const char utf8[] = "blah" "\xf0\x9f\x92\xa9" "blah" "\xed\xa0\xbd" "blah"
                       "\xed\xb2\xa9" "blah" "\0" "blah" "\xe1\x88\xb4";

    Json uni = Json::parse(unicode_escape_test, err);
    JSON11_TEST_ASSERT(uni[0].string_value().size() == (sizeof utf8) - 1);
    JSON11_TEST_ASSERT(std::memcmp(uni[0].string_value().data(), utf8, sizeof utf8) == 0);

#if JSON11_ENABLE_DR1467_CANARY
    Json nested_array = Json::array{Json::array{1, 2, 3}};
    JSON11_TEST_ASSERT(nested_array.is_array());
    JSON11_TEST_ASSERT(nested_array.array_items().size() == 1);
    JSON11_TEST_ASSERT(nested_array.array_items()[0].is_array());
    JSON11_TEST_ASSERT(nested_array.array_items()[0].array_items().size() == 3);
#endif

    // Multi-parse tests
    struct TestMultiParse {
        string input;
        string::size_type expect_parser_stop_pos;
        size_t expect_not_empty_elms_count;
        Json expect_parse_res;
    };

    const TestMultiParse tests[] = {
        {" {", 0, 0, {}},
        {R"({"k1":"v1"})", 13, 1, Json::object{{"k1", "v1"}}},
        {R"({"k1":"v1"} {)", 14, 1, Json::object{{"k1", "v1"}}},
        {R"({"k1":"v1"}{"k2":"v2", "k3":[)", 13, 1, Json::object{{"k1", "v1"}}},
        {"{}", 2, 1, Json::object{}}
    };

    for (const auto& tst : tests) {
        string::size_type parser_stop_pos;
        string err;
        auto res = Json::parse_multi(tst.input, parser_stop_pos, err);
        
        JSON11_TEST_ASSERT(parser_stop_pos == tst.expect_parser_stop_pos);
        JSON11_TEST_ASSERT(
            std::count_if(res.begin(), res.end(),
                       [](const Json& j) { return !j.is_null(); })
            == tst.expect_not_empty_elms_count);
        
        if (!res.empty()) {
            JSON11_TEST_ASSERT(tst.expect_parse_res == res[0]);
        }
    }

    // Custom object serialization test
    Json my_json = Json::object{
        {"key1", "value1"},
        {"key2", false},
        {"key3", Json::array{1, 2, 3}},
    };
    string json_obj_str = my_json.dump();
    std::cout << "json_obj_str: " << json_obj_str << "\n";
    JSON11_TEST_ASSERT(json_obj_str == R"({"key1": "value1", "key2": false, "key3": [1, 2, 3]})");

    class Point {
    public:
        int x;
        int y;
        Point(int x, int y) : x(x), y(y) {}
        Json to_json() const { return Json::array{x, y}; }
    };

    std::vector<Point> points = {{1, 2}, {10, 20}, {100, 200}};
    string points_json = Json(points).dump();
    std::cout << "points_json: " << points_json << "\n";
    JSON11_TEST_ASSERT(points_json == "[[1, 2], [10, 20], [100, 200]]");

    // Shape validation tests
    JSON11_TEST_ASSERT(Json(Json::object{{"foo", nullptr}}).has_shape({{{"foo", Json::Type::NUL}}}, err));
    JSON11_TEST_ASSERT(!Json(Json::object{{"foo", 1234567}}).has_shape({{{"foo", Json::Type::NUL}}}, err));
    JSON11_TEST_ASSERT(!Json(Json::object{{"bar", 1234567}}).has_shape({{{"foo", Json::Type::NUL}}}, err));
}

#if JSON11_TEST_STANDALONE_MAIN

static void parse_from_stdin() {
    string buf;
    string line;
    while (std::getline(std::cin, line)) {
        buf += line + '\n';
    }

    string err;
    auto json = Json::parse(buf, err);
    if (!err.empty()) {
        std::printf("Failed: %s\n", err.c_str());
    } else {
        std::printf("Result: %s\n", json.dump().c_str());
    }
}

int main(int argc, char **argv) {
    if (argc == 2 && argv[1] == string("--stdin")) {
        parse_from_stdin();
        return 0;
    }

    json11_test();
}

#endif // JSON11_TEST_STANDALONE_MAIN

JSON11_TEST_CPP_SUFFIX_CODE