///! Simple JSON parsing library with a focus on a simple, usable API.
const std = @import("std");

const Allocator = std.mem.Allocator;

/// Enable to get debug logging during parsing
/// TODO: Probably...consider std.log?
const DEBUG = false;

/// RFC8259 - quotation mark
const TOKEN_DOUBLE_QUOTE = '"';
/// JSON5.5 - single tick
const TOKEN_SINGLE_QUOTE = '\'';

/// RFC8259.2 - begin-object
const TOKEN_CURLY_BRACKET_OPEN = '{';
/// RFC8259.2 - end-object
const TOKEN_CURLY_BRACKET_CLOSE = '}';
/// RFC8259.2 - name-separator
const TOKEN_COLON = ':';

/// RFC8259.2 - begin-array
const TOKEN_BRACKET_OPEN = '[';
/// RFC8259.2 - end-array
const TOKEN_BRACKET_CLOSE = ']';
/// RFC8259.2 - value-separator
const TOKEN_COMMA = ',';

/// RFC8259.2 - Horizonal tab
const TOKEN_HORIZONTAL_TAB = '\u{09}';
/// RFC8259.2 - New line / line feed
const TOKEN_NEW_LINE = '\u{0A}';
/// JSON5.8 - Vertical tab
const TOKEN_VERTICAL_TAB = '\u{0B}';
/// JSON5.8 - Form feed
const TOKEN_FORM_FEED = '\u{0C}';
/// RFC8259.2 - Carriage return
const TOKEN_CARRIAGE_RETURN = '\u{0D}';
/// RFC8259.2 - Space
const TOKEN_SPACE = '\u{20}';
/// JSON5.8 - Non-breaking space
const TOKEN_NON_BREAKING_SPACE = '\u{A0}';
/// JSON5.8 - Line separator
const TOKEN_LINE_SEPARATOR = '\u{2028}';
/// JSON5.8 - Paragraph separator
const TOKEN_PARAGRAPH_SEPARATOR = '\u{2029}';
/// JSON5.8 - Byte order mark
const TOKEN_BOM = '\u{FEFF}';

/// RFC8259.6 - Zero
const TOKEN_ZERO = '0';
/// RFC8259.6 - Minus
const TOKEN_MINUS = '-';
/// RFC8259.6 - Plus
const TOKEN_PLUS = '+';
/// RFC8259.6 - Decimal-point
const TOKEN_PERIOD = '.';
/// RFC8259.6 - Exp e
const TOKEN_EXPONENT_LOWER = 'e';
/// RFC8259.6 - Exp E
const TOKEN_EXPONENT_UPPER = 'E';

/// RFC8259.7 - Reverse solidus
const TOKEN_REVERSE_SOLIDUS = '\\';

/// RFC8259.3 - true value
const TOKEN_TRUE = "true";
/// RFC8259.3 - false value
const TOKEN_FALSE = "false";
/// RFC8259.3 - null value
const TOKEN_NULL = "null";
/// JSON5.6 - infinity
const TOKEN_INFINITY = "Infinity";
/// JSON5.6 - not-a-number
const TOKEN_NAN = "NaN";

/// JSON5.9.1 / ECMA Script 5.1-7.6 - Identifier Starting Character
const TOKEN_DOLLAR_SIGN = '$';
/// JSON5.9.1 / ECMA Script 5.1-7.6 - Identifier Starting Character
const TOKEN_UNDERSCORE = '_';
/// JSON5.7 - Solidus
const TOKEN_SOLIDUS = '/';
/// JSON5.7 - Asterisk
const TOKEN_ASTERISK = '*';
/// JSON5.9.1 / ECMA Script 5.1-7.6 - Identifier Part
const TOKEN_ZERO_WIDTH_NON_JOINER = 0x200C;
/// JSON5.9.1 / ECMA Script 5.1-7.6 - Identifier Part
const TOKEN_ZERO_WIDTH_JOINER = 0x200D;

/// Parser specific errors
pub const ParseError = error{
    /// Returned when failing to determine the type of value to parse
    ParseValueError,
    /// Returned when failing to parse an object
    ParseObjectError,
    /// Returned when failing to parse a number
    ParseNumberError,
    /// Returned when failing to parse a string
    ParseStringError,
    /// Returned when failing to parse an array
    ParseCommentError,
    // Returned when an unexpected token is found (generally when we're expecting something else)
    UnexpectedTokenError,
    // std.unicode
    CodepointTooLarge,
    Utf8CannotEncodeSurrogateHalf,
};

/// All parser errors including allocation, and int/float parsing errors.
pub const ParseErrors = ParseError || Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError;

/// Allows callers to configure which parser style to use.
pub const ParserConfig = struct { parserType: ParserType = ParserType.rfc8259 };

/// Enumerator for the JSON parser type.
pub const ParserType = enum {
    rfc8259,
    json5,
};

/// The type of encoding used for a JSON number
const NumberEncoding = enum {
    integer,
    float,
    exponent,
    hex,
};

pub const JsonIndent = enum {
    NO_LINE,
    SPACES_2,
    SPACES_4,
    TABS,
};

fn serializerWriteIndent(writer: anytype, indentKind: JsonIndent, depth: usize) !void {
    const indent = switch (indentKind) {
        .NO_LINE => return,
        .SPACES_2 => "  ",
        .SPACES_4 => "    ",
        .TABS => "\t",
    };
    for (0..depth) |_| {
        try writer.writeAll(indent);
    }
}

pub const JsonValue = union(enum) {
    nil: void,
    boolean: bool,
    integer: i64,
    float: f64,
    string: []const u8,
    array: *std.ArrayList(JsonValue),
    object: *std.StringArrayHashMap(JsonValue),
    static_string: []const u8,

    pub fn deinit(self: JsonValue, allocator: Allocator) void {
        switch (self) {
            .string => |str| allocator.free(str),
            .array => |arr| {
                for (arr.items) |*item| item.deinit(allocator);
                arr.deinit();
                allocator.destroy(arr);
            },
            .object => |obj| {
                var iter = obj.iterator();
                while (iter.next()) |entry| {
                    allocator.free(entry.key_ptr.*);
                    entry.value_ptr.*.deinit(allocator);
                }
                obj.deinit();
                allocator.destroy(obj);
            },
            else => {},
        }
    }

    /// Handy pass-thru to typed get(...) calls
    pub fn get(self: JsonValue, index: anytype) JsonValue {
        return switch (self) {
            // Figure out a better way to do this
            .object => |obj| if (@TypeOf(index) != usize and @TypeOf(index) != comptime_int) obj.get(index) orelse @panic("No such key") else @panic("Invalid key type"),
            .array => |arr| if (@TypeOf(index) == usize or @TypeOf(index) == comptime_int) arr.items[index] else @panic("Invalid key type"),
            .nil => @panic("Cannot get() from a null value"),
            else => |t| std.debug.panic("'{s}' type doesn't support get()", .{@tagName(t)}),
        };
    }

    /// Handy pass-thru to typed set(...) calls
    pub fn set(self: JsonValue, index: anytype, value: JsonValue) !void {
        switch (self) {
            // Figure out a better way to do this
            .object => |obj| if (@TypeOf(index) != usize and @TypeOf(index) != comptime_int) {
                if (obj.getEntry(index)) |entry| {
                    entry.value_ptr.deinit(obj.allocator);
                    _ = try obj.fetchPut(index, value);
                } else {
                    const key_copy = try obj.allocator.dupe(u8, index);
                    errdefer obj.allocator.free(key_copy);
                    _ = try obj.fetchPut(key_copy, value);
                }
            } else @panic("Invalid key type"),
            .array => |arr| if (@TypeOf(index) == usize or @TypeOf(index) == comptime_int) {
                if (arr.items.len < index) return error.OutOfBounds;
                var old = arr.items[index];
                arr.items[index] = value;
                old.deinit(arr.allocator);
            } else @panic("Invalid key type"),
            .nil => @panic("Cannot set() on a null value"),
            else => |t| std.debug.panic("'{s}' type doesn't support set()", .{@tagName(t)}),
        }
    }

    pub fn setWith(self: JsonValue, index: anytype, value: JsonValue) !JsonValue {
        switch (self) {
            // Figure out a better way to do this
            .object => |obj| if (@TypeOf(index) != usize and @TypeOf(index) != comptime_int) {
                if (obj.getEntry(index)) |entry| {
                    entry.value_ptr.deinit(obj.allocator);
                    _ = try obj.fetchPut(index, value);
                } else {
                    const key_copy = try obj.allocator.dupe(u8, index);
                    errdefer obj.allocator.free(key_copy);
                    _ = try obj.fetchPut(key_copy, value);
                }
                return value;
            } else @panic("Invalid key type"),
            .nil => @panic("Cannot set() on a null value"),
            else => |t| std.debug.panic("'{s}' type doesn't support setWith()", .{@tagName(t)}),
        }
    }

    /// Handy pass-thru to typed append(...) calls
    pub fn append(self: JsonValue, value: JsonValue) !void {
        return switch (self) {
            .array => |arr| try arr.append(value),
            .nil => @panic("Cannot append() to a null value"),
            else => |t| std.debug.panic("'{s}' type doesn't support append()", .{@tagName(t)}),
        };
    }

    /// Handy pass-thru to typed len(...) calls
    pub fn len(self: JsonValue) usize {
        return switch (self) {
            // Figure out a better way to do this
            .object => |obj| obj.count(),
            .array => |arr| arr.items.len,
            .string, .static_string => |str| str.len,
            else => |t| std.debug.panic("'{s}' type doesn't support len()", .{@tagName(t)}),
        };
    }

    /// Returns the string value or panics
    pub fn asString(self: JsonValue) []const u8 {
        return switch (self) {
            .static_string, .string => |s| s,
            else => @panic("Not a string"),
        };
    }

    /// Returns the object value or panics
    pub fn asObject(self: JsonValue) *std.StringArrayHashMap(JsonValue) {
        return if (self == .object) self.object else @panic("Not an object");
    }

    /// Returns the integer value or panics
    pub fn asInteger(self: JsonValue) i64 {
        return if (self == .integer) self.integer else @panic("Not an number");
    }

    /// Returns the float value or panics
    pub fn asFloat(self: JsonValue) f64 {
        return if (self == .float) self.float else @panic("Not an float");
    }

    /// Returns the array value or panics
    pub fn asArray(self: JsonValue) *std.ArrayList(JsonValue) {
        return if (self == .array) self.array else @panic("Not an array");
    }

    /// Returns the array value or panics
    pub fn asBoolean(self: JsonValue) bool {
        return if (self == .boolean) self.boolean else @panic("Not a boolean");
    }

    /// Returns the string value or null
    pub fn stringOrNull(self: JsonValue) ?[]const u8 {
        return switch (self) {
            .static_string, .string => |s| s,
            else => null,
        };
    }

    /// Returns the object value or null
    pub fn objectOrNull(self: JsonValue) ?*std.StringArrayHashMap(JsonValue) {
        return if (self == .object) self.object else null;
    }

    /// Returns the integer value or null
    pub fn integerOrNull(self: JsonValue) ?i64 {
        return if (self == .integer) self.integer else null;
    }

    /// Returns the float value or null
    pub fn floatOrNull(self: JsonValue) ?f64 {
        return if (self == .float) self.float else null;
    }

    /// Returns the array value or null
    pub fn arrayOrNull(self: JsonValue) ?*std.ArrayList(JsonValue) {
        return if (self == .array) self.array else null;
    }

    /// Returns the boolean value or null
    pub fn booleanOrNull(self: JsonValue) ?bool {
        return if (self == .boolean) self.boolean else null;
    }

    /// Serialize the JSON value to a writer
    pub fn serialize(self: JsonValue, writer: anytype, indent: JsonIndent, depth: usize) anyerror!void {
        switch (self) {
            .integer => |i| {
                if (depth > 0) try serializerWriteIndent(writer, indent, depth);
                try writer.print("{d}", .{i});
            },
            .float => |f| try writer.print("{d}", .{f}),
            .string, .static_string => |s| try writer.print("\"{s}\"", .{s}),
            .boolean => |b| try writer.print("{any}", .{b}),
            .nil => try writer.print("null", .{}),
            .object => |o| {
                try writer.writeAll("{");
                if (indent != .NO_LINE) try writer.writeAll("\n");
                var iter = o.iterator();
                var i: usize = 0;
                const size = o.count();
                while (iter.next()) |entry| {
                    const key = entry.key_ptr.*;
                    const value = entry.value_ptr.*;
                    try serializerWriteIndent(writer, indent, depth + 1);
                    try writer.print("\"{s}\": ", .{key});
                    try value.serialize(writer, indent, depth + 1);
                    i += 1;
                    if (i < size) try writer.writeAll(",");
                    if (indent != .NO_LINE) try writer.writeAll("\n");
                }
                try serializerWriteIndent(writer, indent, depth);
                try writer.writeAll("}");
            },
            .array => |a| {
                try writer.writeAll("[");
                if (indent != .NO_LINE) try writer.writeAll("\n");
                for (a.items, 0..) |value, index| {
                    try serializerWriteIndent(writer, indent, depth + 1);
                    try value.serialize(writer, indent, depth + 1);
                    if (index < a.items.len - 1) try writer.writeAll(",");
                    if (indent != .NO_LINE) try writer.writeAll("\n");
                }
                try serializerWriteIndent(writer, indent, depth);
                try writer.writeAll("]");
            },
        }
    }
};

pub const JsonRoot = struct {
    allocator: Allocator,
    value: JsonValue,

    pub fn init(allocator: Allocator, value: JsonValue) JsonRoot {
        return .{
            .allocator = allocator,
            .value = value,
        };
    }

    pub fn deinit(self: *JsonRoot) void {
        self.value.deinit(self.allocator);
    }

    pub fn newObject(self: *JsonRoot) !JsonValue {
        const ptr = try self.allocator.create(std.StringArrayHashMap(JsonValue));
        ptr.* = std.StringArrayHashMap(JsonValue).init(self.allocator);
        return JsonValue{ .object = ptr };
    }

    pub fn newArray(self: *JsonRoot) !JsonValue {
        const ptr = try self.allocator.create(std.ArrayList(JsonValue));
        ptr.* = std.ArrayList(JsonValue).init(self.allocator);
        return JsonValue{ .array = ptr };
    }
};

pub const CONFIG_RFC8259 = ParserConfig{ .parserType = ParserType.rfc8259 };
pub const CONFIG_JSON5 = ParserConfig{ .parserType = ParserType.json5 };

/// "Constant" for JSON true value
var JSON_TRUE = JsonValue{ .boolean = true };

/// "Constant" for JSON false value
var JSON_FALSE = JsonValue{ .boolean = false };

/// "Constant" for JSON null value
var JSON_NULL = JsonValue{ .nil = @as(void, undefined) };

/// "Constant" for JSON positive infinity
var JSON_POSITIVE_INFINITY = JsonValue{ .float = std.math.inf(f64) };

/// "Constant" for JSON negative infinity
var JSON_NEGATIVE_INFINITY = JsonValue{ .float = -std.math.inf(f64) };

/// "Constant" for JSON positive NaN
var JSON_POSITIVE_NAN = JsonValue{ .float = std.math.nan(f64) };

/// "Constant" for JSON negative NaN
var JSON_NEGATIVE_NAN = JsonValue{ .float = -std.math.nan(f64) };

/// Parse a JSON5 string using the provided allocator
pub fn parse(allocator: Allocator, jsonString: []const u8) !JsonRoot {
    _, const value = try parseValue(allocator, jsonString, CONFIG_RFC8259);
    return JsonRoot.init(allocator, value);
}

/// Parse a JSON5 string using the provided allocator
pub fn parseJson5(allocator: Allocator, jsonString: []const u8) !JsonRoot {
    _, const value = try parseValue(allocator, jsonString, CONFIG_JSON5);
    return JsonRoot.init(allocator, value);
}

/// Parse a JSON value from the provided slice
/// Returns the index of the next character to read
fn parseValue(allocator: Allocator, buffer: []const u8, comptime config: ParserConfig) ParseErrors!struct { usize, JsonValue } {
    if (buffer.len == 0) return error.ParseValueError;
    var pos: usize = try trimLeftWhitespace(buffer, config);
    const char = buffer[pos];
    const result = result: {
        switch (char) {
            // { indicates an object
            TOKEN_CURLY_BRACKET_OPEN => {
                const read_pos, const result = try parseObject(allocator, buffer[pos..], config);
                pos += read_pos;
                break :result result;
            },
            // [ indicates an array
            TOKEN_BRACKET_OPEN => {
                const read_pos, const result = try parseArray(allocator, buffer[pos..], config);
                pos += read_pos;
                break :result result;
            },
            // " indicates a string
            TOKEN_DOUBLE_QUOTE => {
                const read_pos, const result = try parseStringWithTerminal(allocator, buffer[pos..], config, TOKEN_DOUBLE_QUOTE);
                pos += read_pos;
                break :result result;
            },
            // 0-9|- indicates a number
            TOKEN_INFINITY[0], TOKEN_NAN[0], '-', '+', 48...57 => {
                const read_pos, const result = try parseNumber(allocator, buffer[pos..], config);
                pos += read_pos;
                break :result result;
            },
            // t indicates true
            TOKEN_TRUE[0] => {
                if (buffer.len < 4) return error.ParseValueError;
                pos += try expectWord(buffer[pos..], TOKEN_TRUE);
                try expectNothingNext(buffer[pos..], config);
                break :result JSON_TRUE;
            },
            // f indicates false
            TOKEN_FALSE[0] => {
                if (buffer.len < 5) return error.ParseValueError;
                pos += try expectWord(buffer[pos..], TOKEN_FALSE);
                try expectNothingNext(buffer[pos..], config);
                break :result JSON_FALSE;
            },
            // n indicates null
            TOKEN_NULL[0] => {
                if (buffer.len < 4) return error.ParseValueError;
                pos += try expectWord(buffer[pos..], TOKEN_NULL);
                try expectNothingNext(buffer[pos..], config);
                break :result JSON_NULL;
            },
            // ' indicates a string (json5)
            TOKEN_SINGLE_QUOTE => {
                if (config.parserType != ParserType.json5) return error.ParseValueError;
                const read_pos, const result = try parseStringWithTerminal(allocator, buffer[pos..], config, TOKEN_SINGLE_QUOTE);
                pos += read_pos;
                break :result result;
            },
            else => {
                var remaining = buffer[pos..];
                if (remaining.len > 16) remaining = remaining[0..16];
                debug("Unable to parse value from \"{s}...\"", .{remaining});

                return error.ParseValueError;
            },
        }
    };

    return .{ pos, result };
}

/// Parse a JSON object from the provided slice
/// Returns the index of the next character to read
/// Note: parseObject _assumes_ the leading { has been stripped and jsonString
///  starts after that point.
fn parseObject(allocator: Allocator, buffer: []const u8, comptime config: ParserConfig) ParseErrors!struct { usize, JsonValue } {
    const ptr = try allocator.create(std.StringArrayHashMap(JsonValue));
    ptr.* = std.StringArrayHashMap(JsonValue).init(allocator);
    const jsonValue = JsonValue{ .object = ptr };
    const jsonObject = jsonValue.object;
    errdefer jsonValue.deinit(allocator);

    var wasLastComma = false;
    var closed = false;
    var pos: usize = 0;
    pos += try expect(buffer, config, TOKEN_CURLY_BRACKET_OPEN);
    while (pos < buffer.len) {
        const char = buffer[pos];
        if (char == TOKEN_CURLY_BRACKET_CLOSE) {
            closed = true;
            break;
        }
        // Skip comments
        if (isComment(buffer[pos..])) {
            pos += try skipComment(buffer[pos..]);
            continue;
        }
        if (char == TOKEN_COMMA or isInsignificantWhitespace(char, config)) {
            wasLastComma = char == TOKEN_COMMA or wasLastComma;
            pos += 1;
            continue;
        }

        if (jsonObject.count() > 0 and !wasLastComma) {
            debug("Unexpected token; expected ',' but found a '{c}' instead", .{buffer[pos]});
            return error.UnexpectedTokenError;
        }
        wasLastComma = false;

        var key = key: {
            if (char == TOKEN_DOUBLE_QUOTE) {
                const read_pos, const value = try parseStringWithTerminal(allocator, buffer[pos..], config, TOKEN_DOUBLE_QUOTE);
                pos += read_pos;
                break :key value;
            }
            if (config.parserType == ParserType.json5 and char == TOKEN_SINGLE_QUOTE) {
                const read_pos, const value = try parseStringWithTerminal(allocator, buffer[pos..], config, TOKEN_SINGLE_QUOTE);
                pos += read_pos;
                break :key value;
            }
            if (config.parserType == ParserType.json5 and isStartOfEcmaScript51Identifier(buffer[pos..])) {
                const read_pos, const value = try parseEcmaScript51Identifier(allocator, buffer[pos..]);
                pos += read_pos;
                break :key value;
            }
            return error.ParseObjectError;
        };
        defer key.deinit(allocator);

        pos += try expect(buffer[pos..], config, TOKEN_COLON);

        const key_string = try allocator.dupe(u8, key.asString());
        errdefer allocator.free(key_string);

        const read_pos, const value = try parseValue(allocator, buffer[pos..], config);
        errdefer value.deinit(allocator);
        pos += read_pos;
        try jsonObject.put(key_string, value);
    }

    if (!closed) {
        debug("Unexpected end of object; expected '}'", .{});
        return error.UnexpectedTokenError;
    }

    // Account for the terminal character
    pos += 1;

    return .{ pos, jsonValue };
}

/// Parse a JSON array from the provided slice
/// Returns the index of the next character to read
/// Note: parseArray _assumes_ the leading [ has been stripped and jsonString
///  starts after that point.
fn parseArray(allocator: Allocator, buffer: []const u8, comptime config: ParserConfig) ParseErrors!struct { usize, JsonValue } {
    const ptr = try allocator.create(std.ArrayList(JsonValue));
    ptr.* = std.ArrayList(JsonValue).init(allocator);
    const jsonValue = JsonValue{ .array = ptr };
    const jsonArray = jsonValue.array;
    errdefer jsonValue.deinit(allocator);

    // Flag to indicate if we've already seen a comma
    var wasLastComma = false;
    var pos: usize = try expect(buffer, config, TOKEN_BRACKET_OPEN);
    while (pos < buffer.len and buffer[pos] != TOKEN_BRACKET_CLOSE) {
        // Skip comments
        if (isComment(buffer[pos..])) {
            pos += try skipComment(buffer[pos..]);
            continue;
        }

        // Skip commas and insignificant whitespaces
        if (buffer[pos] == TOKEN_COMMA or isInsignificantWhitespace(buffer[pos], config)) {
            wasLastComma = buffer[pos] == TOKEN_COMMA or wasLastComma;
            pos += 1;
            continue;
        }

        wasLastComma = false;
        const read_pos, const value = try parseValue(allocator, buffer[pos..], config);
        errdefer value.deinit(allocator);
        pos += read_pos;
        try jsonArray.append(value);
    }

    if (wasLastComma and config.parserType != ParserType.json5) return error.UnexpectedTokenError;

    // Account for the terminal character
    pos += 1;

    return .{ pos, jsonValue };
}

fn findNextChar(buffer: []const u8, target: []const u8) usize {
    for (buffer, 0..) |char, pos| {
        for (target) |fchar| if (char == fchar) return pos;
    }
    return buffer.len;
}

/// Parse a string from the provided slice
/// Returns the index of the next character to read
fn parseStringWithTerminal(allocator: Allocator, buffer: []const u8, comptime config: ParserConfig, terminal: u8) ParseErrors!struct { usize, JsonValue } {
    const ipos = try expectUpTo(buffer, config, terminal);
    var characters = std.ArrayList(u8).init(allocator);
    defer characters.deinit();

    var pos: usize = ipos;
    var last_pos: usize = pos;
    while (pos < buffer.len) {
        pos += findNextChar(buffer[pos..], &[_]u8{ TOKEN_REVERSE_SOLIDUS, terminal });
        const c = buffer[pos];
        if (c == terminal) {
            if (last_pos < pos) try characters.appendSlice(buffer[last_pos..pos]);
            break;
        }
        switch (c) {
            TOKEN_REVERSE_SOLIDUS => {
                defer last_pos = pos;
                try characters.appendSlice(buffer[last_pos..pos]);
                if (pos + 1 >= buffer.len) return error.ParseStringError;
                const nc = buffer[pos + 1];
                pos += 2;
                switch (nc) {
                    // Codepoint
                    'u' => {
                        if (buffer.len < pos + 4) return error.ParseStringError;
                        const intValue = try std.fmt.parseInt(u21, buffer[pos .. pos + 4], 16);
                        var buf: [4]u8 = undefined;
                        const len = try std.unicode.utf8Encode(intValue, &buf);
                        try characters.appendSlice(buf[0..len]);
                        pos += 4;
                    },
                    // Double backslash
                    // Control characters
                    'b' => try characters.append(8),
                    't' => try characters.append(9),
                    'n' => try characters.append(10),
                    'f' => try characters.append(12),
                    'r' => try characters.append(13),
                    '"' => try characters.append('"'),
                    '\'' => try characters.append('\''),
                    '\\' => try characters.append('\\'),
                    else => return error.ParseStringError,
                }
            },
            else => {
                defer last_pos = pos;
                try characters.appendSlice(buffer[last_pos..pos]);
            },
        }
    }

    if (pos >= buffer.len) return error.ParseStringError;

    pos += 1;

    return .{ pos, .{ .string = try allocator.dupe(u8, characters.items) } };
}

/// Parse a number from the provided slice
/// Returns the index of the next character to read
fn parseNumber(allocator: Allocator, buffer: []const u8, comptime config: ParserConfig) ParseErrors!struct { usize, JsonValue } {
    var encodingType = NumberEncoding.integer;
    var pos = try trimLeftWhitespace(buffer, config);
    var startingDigitAt: usize = 0;
    var polarity: isize = 1;
    var numberList = std.ArrayList(u8).init(allocator);
    defer numberList.deinit();

    if (buffer[pos..].len < 1) {
        debug("Number cannot be zero length", .{});
        return error.ParseNumberError;
    }

    const new_buffer = buffer[pos..];

    // First character can be a minus or number

    switch (config.parserType) {
        .json5 => if (isPlusOrMinus(new_buffer[0])) {
            polarity = if (new_buffer[0] == TOKEN_MINUS) -1 else 1;
            try numberList.append(new_buffer[0]);
            startingDigitAt += 1;
            pos += 1;
        },
        .rfc8259 => if (new_buffer[0] == TOKEN_MINUS) {
            polarity = -1;
            try numberList.append(new_buffer[0]);
            startingDigitAt += 1;
            pos += 1;
        },
    }

    if (new_buffer.len <= startingDigitAt) {
        if (startingDigitAt > 0) debug("Invalid number; cannot be just + or -", .{});
        return error.ParseNumberError;
    }

    if (comptime config.parserType == .json5) switch (buffer[pos]) {
        TOKEN_NAN[0] => {
            pos += try expectWord(buffer[pos..], TOKEN_NAN);
            return .{ pos, if (polarity > 0) JSON_POSITIVE_NAN else JSON_NEGATIVE_NAN };
        },
        TOKEN_INFINITY[0] => {
            pos += try expectWord(buffer[pos..], TOKEN_INFINITY);
            return .{ pos, if (polarity > 0) JSON_POSITIVE_INFINITY else JSON_NEGATIVE_INFINITY };
        },
        else => {},
    };

    // Next character either is a digit or a .

    switch (buffer[pos]) {
        '0' => {
            try numberList.append('0');
            pos += 1;
            if (pos < buffer.len) {
                switch (buffer[pos]) {
                    TOKEN_ZERO => {
                        debug("Invalid number; number cannot start with multiple zeroes", .{});
                        return error.ParseNumberError;
                    },
                    'x' => {
                        encodingType = NumberEncoding.hex;
                        try numberList.append('x');
                        pos += 1;
                    },
                    else => {},
                }
            }
        },
        49...57 => {
            try numberList.append(buffer[pos]);
            pos += 1;
        },
        TOKEN_PERIOD => {
            if (comptime config.parserType == ParserType.rfc8259) {
                debug("Invalid number; RFS8259 doesn't support floating point numbers starting with a decimal point", .{});
                return error.ParseNumberError;
            }

            encodingType = NumberEncoding.float;
            try numberList.append(buffer[pos]);
            pos += 1;
            if (pos >= buffer.len) {
                debug("Invalid number; decimal value must follow decimal point", .{});
                return error.ParseNumberError;
            }
        },
        else => {
            debug("Invalid number; invalid starting character, '{c}'", .{buffer[pos]});
            return error.ParseNumberError;
        },
    }

    // Walk through each character
    while (pos < buffer.len and ((encodingType != NumberEncoding.hex and isNumber(buffer[pos])) or (encodingType == NumberEncoding.hex and isHexDigit(buffer[pos])))) {
        try numberList.append(buffer[pos]);
        pos += 1;
    }

    // Handle decimal numbers
    if (pos < buffer.len and encodingType != NumberEncoding.hex and buffer[pos] == TOKEN_PERIOD) {
        encodingType = NumberEncoding.float;
        try numberList.append(buffer[pos]);
        pos += 1;
        while (pos < buffer.len and isNumber(buffer[pos])) {
            try numberList.append(buffer[pos]);
            pos += 1;
        }
    }

    // Handle exponent
    if (pos < buffer.len and encodingType != NumberEncoding.hex and (buffer[pos] == TOKEN_EXPONENT_LOWER or buffer[pos] == TOKEN_EXPONENT_UPPER)) {
        encodingType = NumberEncoding.float;
        try numberList.append(buffer[pos]);
        pos += 1;
        if (!isNumberOrPlusOrMinus(buffer[pos])) {
            return error.ParseNumberError;
        }
        // Handle preceeding +/-
        try numberList.append(buffer[pos]);
        pos += 1;
        // Handle the exponent value
        while (pos < buffer.len and isNumber(buffer[pos])) {
            try numberList.append(buffer[pos]);
            pos += 1;
        }
    }

    if (pos > buffer.len) @panic("Fail");

    switch (encodingType) {
        NumberEncoding.integer => return .{ pos, .{ .integer = try std.fmt.parseInt(i64, numberList.items, 10) } },
        NumberEncoding.float => return .{ pos, .{ .float = try std.fmt.parseFloat(f64, numberList.items) } },
        // parseInt doesn't support 0x so we have to skip it and manually apply the sign
        NumberEncoding.hex => {
            return .{ pos, .{ .integer = polarity * try std.fmt.parseInt(i64, numberList.items[startingDigitAt + 2 .. numberList.items.len], 16) } };
        },
        else => return error.ParseNumberError,
    }
}

// TODO: Drop the JsonValue return
fn parseEcmaScript51Identifier(allocator: Allocator, buffer: []const u8) ParseErrors!struct { usize, JsonValue } {
    var characters = std.ArrayList(u8).init(allocator);
    defer characters.deinit();

    var pos: usize = 0;
    while (pos < buffer.len) : (pos += 1) {
        const c = buffer[pos];
        if (!isValidEcmaScript51IdentifierCharacter(buffer[pos..])) {
            debug("Invalid character in EcmaScript51Identifier: '{c}'", .{c});
            break;
        }

        switch (c) {
            TOKEN_REVERSE_SOLIDUS => {
                // Unicode escaped character
                if (buffer.len < pos + 6) return error.ParseStringError;
                if (buffer[pos + 1] != 'u') return error.ParseStringError;
                pos += 2;

                const intValue = try std.fmt.parseInt(u21, buffer[pos .. pos + 4], 16);
                var buf: [4]u8 = undefined;
                const len = try std.unicode.utf8Encode(intValue, &buf);
                try characters.appendSlice(buf[0..len]);
                pos += 3;
            },
            else => try characters.append(c),
        }
    }

    if (pos > buffer.len) return error.ParseStringError;

    return .{ pos, .{ .string = try allocator.dupe(u8, characters.items) } };
}

/// Expects the next significant character be token, skipping over all leading and trailing
/// insignificant whitespace, or returns UnexpectedTokenError.
fn expect(buffer: []const u8, comptime config: ParserConfig, token: u8) ParseErrors!usize {
    var pos = try trimLeftWhitespace(buffer, config);
    const new_buffer = buffer[pos..];
    if (new_buffer.len < 1) {
        debug("Expected {c} found EOF", .{token});
        return error.UnexpectedTokenError;
    }
    if (new_buffer[0] != token) {
        debug("Expected {c} found {c}", .{ token, buffer[0] });
        return error.UnexpectedTokenError;
    }
    pos += 1;
    pos += try trimLeftWhitespace(buffer[pos..], config);
    return pos;
}

/// Expects the next significant character be token, skipping over all leading insignificant
/// whitespace, or returns UnexpectedTokenError.
fn expectUpTo(buffer: []const u8, comptime config: ParserConfig, token: u8) ParseErrors!usize {
    const pos = try trimLeftWhitespace(buffer, config);
    if (buffer[pos] != token) {
        debug("Expected {c} found {c}", .{ token, buffer[pos] });
        return error.UnexpectedTokenError;
    }
    return pos + 1;
}

/// Returns the index in the string with the next, significant character
/// starting from the beginning.
fn trimLeftWhitespace(buffer: []const u8, comptime config: ParserConfig) ParseErrors!usize {
    // Skip any whitespace
    const pos = pos: {
        for (buffer, 0..) |c, p| if (!isInsignificantWhitespace(c, config)) break :pos p;
        break :pos buffer.len;
    };

    // Skip any comments
    const new_buffer = buffer[pos..];
    if (config.parserType == ParserType.json5 and isComment(new_buffer)) {
        const new_pos = try skipComment(new_buffer);
        return pos + new_pos;
    }

    return pos;
}

/// Skip over comments
fn skipComment(buffer: []const u8) ParseErrors!usize {
    if (!isComment(buffer)) return 0;
    const tokens = buffer[0..2];
    switch (tokens[1]) {
        TOKEN_SOLIDUS => {
            const pos = pos: {
                for (buffer, 0..) |c, p| if (c == TOKEN_NEW_LINE) break :pos p;
                break :pos buffer.len;
            };
            return pos + 1;
        },
        TOKEN_ASTERISK => {
            const pos = pos: {
                for (buffer, 0..) |c, p| {
                    if (p + 1 >= buffer.len) return error.ParseCommentError;
                    if (c == TOKEN_ASTERISK and buffer[p + 1] == TOKEN_SOLIDUS) break :pos p;
                }
                break :pos buffer.len;
            };
            if (pos + 2 >= buffer.len) return error.ParseCommentError;
            return pos + 2;
        },
        else => unreachable, // isComment checks for this
    }
}

/// Returns true if jsonString starts with a comment
fn isComment(buffer: []const u8) bool {
    if (buffer.len < 2) return false;
    if (buffer[0] != TOKEN_SOLIDUS) return false;
    switch (buffer[1]) {
        TOKEN_SOLIDUS, TOKEN_ASTERISK => return true,
        else => return false,
    }
}

/// Returns true if a character matches the RFC8259 grammar specificiation for
/// insignificant whitespace.
fn isInsignificantWhitespace(char: u8, comptime config: ParserConfig) bool {
    if (config.parserType == ParserType.rfc8259) {
        switch (char) {
            TOKEN_HORIZONTAL_TAB, TOKEN_NEW_LINE, TOKEN_CARRIAGE_RETURN, TOKEN_SPACE => return true,
            else => return false,
        }
    }

    switch (@as(u16, @intCast(char))) {
        TOKEN_HORIZONTAL_TAB, TOKEN_NEW_LINE, TOKEN_VERTICAL_TAB, TOKEN_FORM_FEED, TOKEN_CARRIAGE_RETURN, TOKEN_SPACE, TOKEN_NON_BREAKING_SPACE, TOKEN_LINE_SEPARATOR, TOKEN_PARAGRAPH_SEPARATOR, TOKEN_BOM => return true,
        else => return false,
    }

    // TODO: Space Separator Unicode category
}

/// Returns true if the character is a plus or minus
fn isPlusOrMinus(char: u8) bool {
    return char == TOKEN_PLUS or char == TOKEN_MINUS;
}

/// Returns true if the character is a number, minus, or plus
fn isNumberOrPlusOrMinus(char: u8) bool {
    return char == TOKEN_MINUS or char == TOKEN_PLUS or isNumber(char);
}

/// Returns true if the character is a number or minus
fn isNumberOrMinus(char: u8) bool {
    return char == TOKEN_MINUS or isNumber(char);
}

/// Returns true if the character is a number
fn isNumber(char: u8) bool {
    return (char >= 48 and char <= 57);
}

fn isReservedInfinity(buffer: []const u8) bool {
    if (buffer.len < 8) return false;
    return buffer[0] == TOKEN_INFINITY[0];
}

fn isReservedNan(buffer: []const u8) bool {
    if (buffer.len < 3) return false;
    return buffer[0] == TOKEN_NAN[0] and buffer[1] == TOKEN_NAN[1];
}

fn expectWord(buffer: []const u8, word: []const u8) ParseError!usize {
    if (buffer.len < word.len) return error.ParseValueError;
    if (!std.mem.eql(u8, buffer[0..word.len], word)) return error.ParseValueError;
    return word.len;
}

fn expectNothingNext(buffer: []const u8, comptime config: ParserConfig) ParseErrors!void {
    if (buffer.len == 0) return;
    if (isInsignificantWhitespace(buffer[0], config)) return;
    switch (buffer[0]) {
        ',', ']', '}' => {},
        else => return error.ParseValueError,
    }
}

/// Returns true if jsonString starts with an ECMA Script 5.1 identifier
fn isStartOfEcmaScript51Identifier(buffer: []const u8) bool {
    if (buffer.len == 0) return false;
    const char = buffer[0];
    // Allowable Identifier starting characters
    if (char == TOKEN_COLON) return false;
    if (isEcmaScript51IdentifierUnicodeCharacter(char) or char == TOKEN_DOLLAR_SIGN or char == TOKEN_UNDERSCORE) return true;
    if (buffer.len >= 6) {
        return buffer[0] == TOKEN_REVERSE_SOLIDUS and buffer[1] == 'u';
    }

    return false;
}

/// Returns true if the character is an ECMA Script 5.1 identifier unicode character
fn isEcmaScript51IdentifierUnicodeCharacter(char: u8) bool {
    return char >= 0x0041 and char <= 0x1E921;
}

/// Returns true if the character is an ECMA Script 5.1 identifier character
fn isValidEcmaScript51IdentifierCharacter(buffer: []const u8) bool {
    const char = buffer[0];
    if (char == TOKEN_COLON) return false;
    return isStartOfEcmaScript51Identifier(buffer) or isUnicodeDigit(char) or char == TOKEN_ZERO_WIDTH_NON_JOINER or char == TOKEN_ZERO_WIDTH_JOINER;
}

/// Returns true if the character is a unicode digit
fn isUnicodeDigit(char: u8) bool {
    return (char >= 0x0030 and char <= 0x0039)
    // TODO: Finish these...
    or (char >= 0x0660 and char <= 0x0669) or (char >= 0x06F0 and char <= 0x06F9) or (char >= 0x07C0 and char <= 0x07C9) or (char >= 0x0966 and char <= 0x096F) or (char >= 0x09E6 and char <= 0x09EF) or (char >= 0x0A66 and char <= 0x0A6F) or (char >= 0x0AE6 and char <= 0x0AEF) or (char >= 0x0B66 and char <= 0x00BF) or (char >= 0x0BE6 and char <= 0x0BEF) or (char >= 0x0C66 and char <= 0x0C6F) or (char >= 0x0CE6 and char <= 0x0CEF) or (char >= 0x0D66 and char <= 0x0D6F);
}

fn isHexDigit(char: u8) bool {
    return (char >= '0' and char <= '9') or (char >= 'A' and char <= 'F') or (char >= 'a' and char <= 'f');
}

/// Helper for printing messages
fn debug(comptime msg: []const u8, args: anytype) void {
    if (DEBUG) {
        std.debug.print(msg, args);
        std.debug.print("\n", .{});
    }
}

/// Helper for testing parsed numbers - only calls parseNumber
/// number can be an expected number or an expected error
fn expectParseNumberToParseNumber(number: anytype, text: []const u8, comptime config: ParserConfig) !void {
    const allocator = std.testing.allocator;

    var value = v: {
        switch (@typeInfo(@TypeOf(number))) {
            @typeInfo(ParseErrors) => break :v parseNumber(allocator, text, config),
            else => {
                _, const v = try parseNumber(allocator, text, config);
                break :v v;
            },
        }
    };

    switch (@typeInfo(@TypeOf(number))) {
        .Int, @typeInfo(comptime_int) => try std.testing.expect(value == .integer),
        .Float, @typeInfo(comptime_float) => try std.testing.expect(value == .float),
        @typeInfo(ParseErrors) => {},
        else => @compileError("Eek: " ++ @typeName(@TypeOf(number))),
    }

    switch (@typeInfo(@TypeOf(number))) {
        @typeInfo(comptime_int) => try std.testing.expectEqual(@as(i64, number), value.asInteger()),
        .Int => try std.testing.expectEqual(number, value.asInteger()),
        @typeInfo(comptime_float) => try std.testing.expectEqual(@as(f64, number), value.asFloat()),
        .Float => try std.testing.expectEqual(number, value.asFloat()),
        @typeInfo(ParseErrors) => try std.testing.expectError(number, value),
        else => @compileError("Eek: " ++ @typeName(@TypeOf(number))),
    }

    switch (@typeInfo(@TypeOf(number))) {
        @typeInfo(ParseErrors) => {},
        else => {},
    }
}

// Unit Tests
test "parse can parse a number" {
    const allocator = std.testing.allocator;

    var root = try parse(allocator, "0");
    try std.testing.expect(root.value == .integer);
    try std.testing.expectEqual(root.value.asInteger(), 0);

    root.deinit();

    root = try parse(allocator, "0.1");
    try std.testing.expect(root.value == .float);
    try std.testing.expectEqual(root.value.asFloat(), 0.1);

    root.deinit();
}

test "parse can parse a object" {
    const allocator = std.testing.allocator;

    var root = try parse(allocator, "{\"foo\":\"bar\"}");
    defer root.deinit();
    try std.testing.expect(root.value == .object);
}

test "parse can parse a array" {
    const allocator = std.testing.allocator;

    var root = try parse(allocator, "[0,\"foo\",1.337]");
    defer root.deinit();
    try std.testing.expect(root.value == .array);
    try std.testing.expectEqual(root.value.get(0).asInteger(), 0);
    try std.testing.expect(std.mem.eql(u8, root.value.get(1).asString(), "foo"));
    try std.testing.expectEqual(root.value.get(2).asFloat(), 1.337);
}

test "parse can parse an object" {
    const allocator = std.testing.allocator;

    var root = try parse(allocator, "{\"foo\":\"bar\", \"zig\":\"zabim\"}");
    try std.testing.expect(root.value == .object);
    try std.testing.expect(std.mem.eql(u8, root.value.get("foo").asString(), "bar"));
    const keys = root.value.asObject().keys();

    // TODO: Improve these conditions - can't rely on deterministic key ordering
    try std.testing.expectEqual(2, keys.len);
    try std.testing.expectEqualStrings("foo", keys[0]);
    try std.testing.expectEqualStrings("zig", keys[1]);

    root.deinit();
}

test "RFC8259.3: parseValue can parse true" {
    const allocator = std.testing.allocator;

    _, const value = try parseValue(allocator, "true", CONFIG_RFC8259);
    try std.testing.expect(value == .boolean);
    try std.testing.expectEqual(value.asBoolean(), true);

    // Note: true, false, and null are constant JsonValues
    // and should not be destroyed
}

test "RFC8259.3: parseValue can parse false" {
    const allocator = std.testing.allocator;

    _, const value = try parseValue(allocator, "false", CONFIG_RFC8259);
    try std.testing.expect(value == .boolean);
    try std.testing.expectEqual(value.asBoolean(), false);

    // Note: true, false, and null are constant JsonValues
    // and should not be destroyed
}

test "RFC8259.3: parseValue can parse null" {
    const allocator = std.testing.allocator;

    _, const value = try parseValue(allocator, "null", CONFIG_RFC8259);
    try std.testing.expect(value == .nil);
    try std.testing.expect(value == .nil);

    // Note: true, false, and null are constant JsonValues
    // and should not be destroyed
}

test "RFC8259.4: parseObject can parse an empty object /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseValue(allocator, "{}", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .object);
    try std.testing.expectEqual(value.len(), 0);
}

test "RFC8259.4: parseObject can parse an empty object /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseValue(allocator, "{ }", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .object);
    try std.testing.expectEqual(value.len(), 0);
}

test "RFC8259.4: parseObject can parse an empty object /3" {
    const allocator = std.testing.allocator;

    // Create an empty object with all insignificant whitespace characters
    _, const value = try parseValue(allocator, "\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}{\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}}\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .object);
    try std.testing.expectEqual(value.len(), 0);
}

test "RFC8259.4: parseObject can parse a simple object /1" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "{\"key1\": \"foo\", \"key2\": \"foo2\", \"key3\": -1, \"key4\": [], \"key5\": { } }", CONFIG_RFC8259);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("key2"), true);
    try std.testing.expect(jsonResult.get("key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("key3"), true);
    try std.testing.expect(jsonResult.get("key3") == .integer);
    try std.testing.expectEqual(jsonResult.get("key3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "RFC8259.4: parseObject can parse a simple object /2" {
    const allocator = std.testing.allocator;

    // Same text body as /1 but every inbetween character is the set of insignificant whitepsace
    // characters
    _, const jsonResult = try parseObject(allocator, "\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}{\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"key1\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"foo\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"key2\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"foo2\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"key3\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}-1\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"key4\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}[]\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"key5\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}{\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}}\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}}\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}", CONFIG_RFC8259);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("key2"), true);
    try std.testing.expect(jsonResult.get("key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("key3"), true);
    try std.testing.expect(jsonResult.get("key3") == .integer);
    try std.testing.expectEqual(jsonResult.get("key3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "RFC8259.4: parseObject returns UnexpectedTokenException on trailing comma" {
    const allocator = std.testing.allocator;

    // Same text body as /1 but every inbetween character is the set of insignificant whitepsace
    // characters
    const jsonResult = parseObject(allocator, "{\"key1\": 1, \"key2\": \"two\", \"key3\": 3.0, \"key4\", {},}", CONFIG_RFC8259);
    try std.testing.expectError(error.UnexpectedTokenError, jsonResult);
}

test "RFC8259.4: parseObject returns UnexpectedTokenException on missing comma" {
    const allocator = std.testing.allocator;

    // Same text body as /1 but every inbetween character is the set of insignificant whitepsace
    // characters
    const jsonResult = parseObject(allocator, "{\"key1\": 1, \"key2\": \"two\", \"key3\": 3.0, \"key4\" {}}", CONFIG_RFC8259);
    try std.testing.expectError(error.UnexpectedTokenError, jsonResult);
}

test "RFC8259.5: parseArray can parse an empty array /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "[]", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.len(), 0);
}

test "RFC8259.5: parseArray can parse an empty array /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}[\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}]\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.len(), 0);
}

test "RFC8259.5: parseArray can parse an simple array /3" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "[-1,-1.2,0,1,1.2,\"\",\"foo\",true,false,null,{},{\"foo\":\"bar\", \"baz\": {}}]", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.len(), 12);
}

test "RFC8259.5: parseArray can parse an simple array /4" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}[\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}-1\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}-1.2\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}0\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}1\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}1.2\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"foo\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}true\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}false\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}null\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}{\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}}\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}{\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"foo\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"bar\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d},\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"baz\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}:\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}{\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}}\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}}\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}]\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}", CONFIG_RFC8259);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.len(), 12);
}

test "RFC8259.5: parseArray returns UnexpectedTokenError on trailing comma" {
    const allocator = std.testing.allocator;

    const value = parseArray(allocator, "[1,\"two\",3.0,{},]", CONFIG_RFC8259);
    try std.testing.expectError(error.UnexpectedTokenError, value);
}

test "RFC8259.6: parseNumber can parse a integer /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "0", CONFIG_RFC8259);
    try std.testing.expect(value == .integer);
    try std.testing.expectEqual(value.asInteger(), 0);
}

test "RFC8259.6: parseNumber can parse a integer /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "1", CONFIG_RFC8259);
    try std.testing.expect(value == .integer);
    try std.testing.expectEqual(value.asInteger(), 1);
}

test "RFC8259.6: parseNumber can parse a integer /3" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "1337", CONFIG_RFC8259);
    try std.testing.expect(value == .integer);
    try std.testing.expectEqual(value.asInteger(), 1337);
}

test "RFC8259.6: parseNumber can parse a integer /4" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "-1337", CONFIG_RFC8259);
    try std.testing.expect(value == .integer);
    try std.testing.expectEqual(value.asInteger(), -1337);
}

test "RFC8259.6: parseNumber can parse a float /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "1.0", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 1.0);
}

test "RFC8259.6: parseNumber can parse a float /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "-1.0", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), -1.0);
}

test "RFC8259.6: parseNumber can parse a float /3" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "1337.0123456789", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 1337.0123456789);
}

test "RFC8259.6: parseNumber can parse a float /4" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "-1337.0123456789", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), -1337.0123456789);
}

test "RFC8259.6: parseNumber can parse an exponent /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "13e37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 13e37);
}

test "RFC8259.6: parseNumber can parse an exponent /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "13E37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 13E37);
}

test "RFC8259.6: parseNumber can parse an exponent /3" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "13E+37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 13E+37);
}

test "RFC8259.6: parseNumber can parse an exponent /4" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "13E-37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 13E-37);
}

test "RFC8259.6: parseNumber can parse an exponent /5" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "-13e37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), -13e37);
}

test "RFC8259.6: parseNumber can parse an exponent /6" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "-13E37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), -13E37);
}

test "RFC8259.6: parseNumber can parse an exponent /7" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "-13E+37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), -13E+37);
}

test "RFC8259.6: parseNumber can parse an exponent /8" {
    const allocator = std.testing.allocator;

    _, const value = try parseNumber(allocator, "13E-37", CONFIG_RFC8259);
    try std.testing.expect(value == .float);
    try std.testing.expectEqual(value.asFloat(), 13E-37);
}

test "RFC8259.6: parseNumber fails on a repeating 0" {
    const allocator = std.testing.allocator;

    const value = parseNumber(allocator, "00", CONFIG_RFC8259);
    try std.testing.expectError(error.ParseNumberError, value);
}

test "RFC8259.6: parseNumber fails on a non-minus and non-digit start /1" {
    const allocator = std.testing.allocator;

    const value = parseNumber(allocator, "a0", CONFIG_RFC8259);
    try std.testing.expectError(error.ParseNumberError, value);
}

test "RFC8259.6: parseNumber fails on a non-minus and non-digit start /2" {
    const allocator = std.testing.allocator;

    const value = parseNumber(allocator, "+0", CONFIG_RFC8259);
    try std.testing.expectError(error.ParseNumberError, value);
}

test "RFC8259.6: parseNumber fails on number starting with decimal point" {
    const allocator = std.testing.allocator;

    const value = parseNumber(allocator, ".0", CONFIG_RFC8259);
    try std.testing.expectError(error.ParseNumberError, value);
}

test "RFC8259.6 parseNumber ignores multi-line comments /1" {
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */0.0/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */-0.0/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */+0.0/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */0.1/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */.1/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */+.1/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */-.1/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */+0.1/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */-0.1/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */100.0/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */-100.0/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */Infinity/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */-Infinity/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */+Infinity/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */NaN/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */-NaN/* comment */", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "/* comment */+NaN/* comment */", CONFIG_RFC8259);
}

test "RFC8259.6 parseNumber fails on single-line comments /1" {
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n0.0\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n-0.0\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n+0.0\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n0.1\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n.1\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n+.1\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n-.1\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n+0.1\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n-0.1\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n100.0\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n-100.0\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\nInfinity\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n-Infinity\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n+Infinity\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\nNaN\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n-NaN\n// comment", CONFIG_RFC8259);
    try expectParseNumberToParseNumber(error.ParseNumberError, "// comment\n+NaN\n// comment", CONFIG_RFC8259);
}

test "JSON5.7 parseArray ignores multi-line comments /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "/* comment */[/* comment */1/* comment */,/* comment */\"two\"/* comment */,/* comment */3.0/* comment */,/* comment */{/* comment */},/* comment */'five'/* comment */,/* comment */{/* comment */six/* comment */:/* comment */0x07/* comment */}/* comment */]/* comment */", CONFIG_JSON5);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.get(0).asInteger(), 1);
    try std.testing.expect(std.mem.eql(u8, value.get(1).asString(), "two"));
    try std.testing.expectEqual(value.get(2).asFloat(), 3.0);
    try std.testing.expectEqual(value.get(3).len(), 0);
    try std.testing.expect(std.mem.eql(u8, value.get(4).asString(), "five"));
    try std.testing.expectEqual(value.get(5).get("six").asInteger(), 7);
}

test "JSON5.7 parseArray ignores single-line comments /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "// comment \n[// comment \n1// comment \n,// comment \n\"two\"// comment \n,// comment \n3.0// comment \n,// comment \n{// comment \n},// comment \n'five'// comment \n,// comment \n{// comment \nsix// comment \n:// comment \n0x07// comment \n}// comment \n]// comment \n", CONFIG_JSON5);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.get(0).asInteger(), 1);
    try std.testing.expect(std.mem.eql(u8, value.get(1).asString(), "two"));
    try std.testing.expectEqual(value.get(2).asFloat(), 3.0);
    try std.testing.expectEqual(value.get(3).len(), 0);
    try std.testing.expect(std.mem.eql(u8, value.get(4).asString(), "five"));
    try std.testing.expectEqual(value.get(5).get("six").asInteger(), 7);
}

test "JSON5.7: parseObject ignores multi-line comments /1" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "/* comment */{/* comment */key1/* comment */:/* comment */\"foo\"/* comment */,/* comment */ȡkey2/* comment */:/* comment */\"foo2\"/* comment */,/* comment */\u{0221}key3/* comment */:/* comment */-1/* comment */,/* comment */'key4'/* comment */:/* comment */[/* comment */]/* comment */,/* comment */\"key5\"/* comment */:/* comment */{/* comment */}/* comment */,/* comment */}/* comment */", CONFIG_JSON5);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("\u{0221}key2"), true);
    try std.testing.expect(jsonResult.get("\u{0221}key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("\u{0221}key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("ȡkey3"), true);
    try std.testing.expect(jsonResult.get("ȡkey3") == .integer);
    try std.testing.expectEqual(jsonResult.get("ȡkey3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "JSON5.7: parseObject ignores single-line comments /2" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "// comment \n{// comment \nkey1// comment \n:// comment \n\"foo\"// comment \n,// comment \nȡkey2// comment \n:// comment \n\"foo2\"// comment \n,// comment \n\u{0221}key3// comment \n:// comment \n-1// comment \n,// comment \n'key4'// comment \n:// comment \n[// comment \n]// comment \n,// comment \n\"key5\"// comment \n:// comment \n{// comment \n}// comment \n,// comment \n}// comment \n", CONFIG_JSON5);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("\u{0221}key2"), true);
    try std.testing.expect(jsonResult.get("\u{0221}key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("\u{0221}key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("ȡkey3"), true);
    try std.testing.expect(jsonResult.get("ȡkey3") == .integer);
    try std.testing.expectEqual(jsonResult.get("ȡkey3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "RFC8259.7: parseStringWithTerminal can parse an empty string /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseStringWithTerminal(allocator, "\"\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), ""));
}

test "RFC8259.7: parseStringWithTerminal can parse an empty string /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseStringWithTerminal(allocator, "\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}\"\"\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), ""));
}

test "RFC8259.7: parseStringWithTerminal can parse a simple string /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseStringWithTerminal(allocator, "\"some string\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "some string"));
}

test "RFC8259.7: parseStringWithTerminal can parse a simple string /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseStringWithTerminal(allocator, "\"some\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}string\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "some\u{20}\u{09}\u{0A}\u{0a}\u{0D}\u{0d}string"));
}

test "RFC8259.7: parseStringWithTerminal can parse a simple string /3" {
    const allocator = std.testing.allocator;

    // some\"string
    _, const value = try parseStringWithTerminal(allocator, "\"some\\\"string\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "some\"string"));
}

test "RFC8259.7: parseStringWithTerminal can parse a simple string /4" {
    const allocator = std.testing.allocator;

    // some\\"string
    _, const value = try parseStringWithTerminal(allocator, "\"some\\\\\\\"string\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "some\\\"string"));
}

test "RFC8259.7: parseStringWithTerminal can parse a simple string /5" {
    const allocator = std.testing.allocator;

    // ",\,\u{00-0f}
    _, const value = try parseStringWithTerminal(allocator, "\"\\\"\\\\\u{00}\u{01}\u{02}\u{03}\u{04}\u{05}\u{06}\u{07}\u{08}\u{09}\u{0A}\u{0B}\u{0C}\u{0D}\u{0E}\u{0F}\u{10}\u{11}\u{12}\u{13}\u{14}\u{15}\u{16}\u{17}\u{18}\u{19}\u{1A}\u{1B}\u{1C}\u{1D}\u{1E}\u{1F}\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "\"\\\u{00}\u{01}\u{02}\u{03}\u{04}\u{05}\u{06}\u{07}\u{08}\u{09}\u{0A}\u{0B}\u{0C}\u{0D}\u{0E}\u{0F}\u{10}\u{11}\u{12}\u{13}\u{14}\u{15}\u{16}\u{17}\u{18}\u{19}\u{1A}\u{1B}\u{1C}\u{1D}\u{1E}\u{1F}"));
}

test "RFC8259.8.3: parseStringWithTerminal parsing results in equivalent strings" {
    const allocator = std.testing.allocator;

    // Test that \\ equals \u{5C}
    _, const value = try parseStringWithTerminal(allocator, "\"a\\\\b\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "a\u{5C}b"));
}

test "JSON5; parseEcmaScript51Identifier can parse simple identifier /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseEcmaScript51Identifier(allocator, "someIdentifier");
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "someIdentifier"));
}

test "JSON5; parseEcmaScript51Identifier can parse simple identifier /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseEcmaScript51Identifier(allocator, "_someIdentifier");
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "_someIdentifier"));
}

test "JSON5; parseEcmaScript51Identifier can parse simple identifier /3" {
    const allocator = std.testing.allocator;

    _, const value = try parseEcmaScript51Identifier(allocator, "$someIdentifier");
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "$someIdentifier"));
}

test "JSON5.3; parseEcmaScript51Identifier can parse simple identifier /2" {
    const allocator = std.testing.allocator;

    _, const value = try parseEcmaScript51Identifier(allocator, "\\u005FsomeIdentifier");
    defer value.deinit(allocator);
    try std.testing.expect(value == .string);
    try std.testing.expect(std.mem.eql(u8, value.asString(), "\u{005f}someIdentifier"));
}

test "JSON5.3: parseObject can parse a simple object /1" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "{key1: \"foo\", ȡkey2: \"foo2\", \u{0221}key3 : -1, 'key4': [], \"key5\": { } }", CONFIG_JSON5);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("\u{0221}key2"), true);
    try std.testing.expect(jsonResult.get("\u{0221}key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("\u{0221}key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("ȡkey3"), true);
    try std.testing.expect(jsonResult.get("ȡkey3") == .integer);
    try std.testing.expectEqual(jsonResult.get("ȡkey3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "JSON5.3: parseObject can parse a simple object with trailing comma" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "{key1: \"foo\", ȡkey2: \"foo2\", \u{0221}key3 : -1, 'key4': [], \"key5\": { }, }", CONFIG_JSON5);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("\u{0221}key2"), true);
    try std.testing.expect(jsonResult.get("\u{0221}key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("\u{0221}key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("ȡkey3"), true);
    try std.testing.expect(jsonResult.get("ȡkey3") == .integer);
    try std.testing.expectEqual(jsonResult.get("ȡkey3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "JSON5.4 parseArray can parse a simple array /1" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "[1, \"two\", 3.0, {}, 'five', {six: 0x07}]", CONFIG_JSON5);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.get(0).asInteger(), 1);
    try std.testing.expect(std.mem.eql(u8, value.get(1).asString(), "two"));
    try std.testing.expectEqual(value.get(2).asFloat(), 3.0);
    try std.testing.expectEqual(value.get(3).len(), 0);
    try std.testing.expect(std.mem.eql(u8, value.get(4).asString(), "five"));
    try std.testing.expectEqual(value.get(5).get("six").asInteger(), 7);
}

test "JSON5.6 parseNumber can parse an integer" {
    try expectParseNumberToParseNumber(0, "0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0, "-0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0, "+0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(100, "100", CONFIG_JSON5);
    try expectParseNumberToParseNumber(100, "+100", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-100, "-100", CONFIG_JSON5);
}

test "JSON5.6 parseNumber can parse a hex number" {
    try expectParseNumberToParseNumber(0x0, "0x0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "0x00", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "0x000", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "0x0000", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "-0x0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "-0x00", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "-0x000", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "-0x0000", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "+0x0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "+0x00", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "+0x000", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0, "+0x0000", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0123456789ABCDEF, "0x0123456789ABCDEF", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0x0123456789ABCDEF, "-0x0123456789ABCDEF", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0x0123456789ABCDEF, "+0x0123456789ABCDEF", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0xA, "0xA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0xA, "-0xA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0xA, "+0xA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0xAA, "0xAA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0xAA, "-0xAA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0xAA, "+0xAA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0xAAA, "0xAAA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0xAAA, "-0xAAA", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0xAAA, "+0xAAA", CONFIG_JSON5);
}

test "JSON5.4 parseNumber can parse a float" {
    try expectParseNumberToParseNumber(0.0, "0.0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.0, "-0.0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.0, "+0.0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "0.1", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, ".1", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "+.1", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0.1, "-.1", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "+0.1", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0.1, "-0.1", CONFIG_JSON5);
    try expectParseNumberToParseNumber(100.0, "100.0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-100.0, "-100.0", CONFIG_JSON5);
    try expectParseNumberToParseNumber(std.math.inf(f64), "Infinity", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-std.math.inf(f64), "-Infinity", CONFIG_JSON5);
    try expectParseNumberToParseNumber(std.math.inf(f64), "+Infinity", CONFIG_JSON5);
    // No nan checking here because NaN != NaN
}

test "JSON5.6 parseNumber can parse nan" {
    const allocator = std.testing.allocator;

    _, var value = try parseNumber(allocator, "NaN", CONFIG_JSON5);
    try std.testing.expect(value == .float);
    try std.testing.expect(std.math.isNan(value.asFloat()));

    _, value = try parseNumber(allocator, "+NaN", CONFIG_JSON5);
    try std.testing.expect(value == .float);
    try std.testing.expect(std.math.isNan(value.asFloat()));

    _, value = try parseNumber(allocator, "-NaN", CONFIG_JSON5);
    try std.testing.expect(value == .float);
    try std.testing.expect(std.math.isNan(value.asFloat()));
}

test "JSON5.7 parseArray ignores multi-line comments" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "/* comment */[/* comment */1/* comment */,/* comment */\"two\"/* comment */,/* comment */3.0/* comment */,/* comment */{/* comment */},/* comment */'five'/* comment */,/* comment */{/* comment */six/* comment */:/* comment */0x07/* comment */}/* comment */]/* comment */", CONFIG_JSON5);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.get(0).asInteger(), 1);
    try std.testing.expect(std.mem.eql(u8, value.get(1).asString(), "two"));
    try std.testing.expectEqual(value.get(2).asFloat(), 3.0);
    try std.testing.expectEqual(value.get(3).len(), 0);
    try std.testing.expect(std.mem.eql(u8, value.get(4).asString(), "five"));
    try std.testing.expectEqual(value.get(5).get("six").asInteger(), 7);
}

test "JSON5.7 parseArray ignores single-line comments" {
    const allocator = std.testing.allocator;

    _, const value = try parseArray(allocator, "// comment \n[// comment \n1// comment \n,// comment \n\"two\"// comment \n,// comment \n3.0// comment \n,// comment \n{// comment \n},// comment \n'five'// comment \n,// comment \n{// comment \nsix// comment \n:// comment \n0x07// comment \n}// comment \n]// comment \n", CONFIG_JSON5);
    defer value.deinit(allocator);
    try std.testing.expect(value == .array);
    try std.testing.expectEqual(value.get(0).asInteger(), 1);
    try std.testing.expect(std.mem.eql(u8, value.get(1).asString(), "two"));
    try std.testing.expectEqual(value.get(2).asFloat(), 3.0);
    try std.testing.expectEqual(value.get(3).len(), 0);
    try std.testing.expect(std.mem.eql(u8, value.get(4).asString(), "five"));
    try std.testing.expectEqual(value.get(5).get("six").asInteger(), 7);
}

test "JSON5.7: parseObject ignores multi-line comments" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "/* comment */{/* comment */key1/* comment */:/* comment */\"foo\"/* comment */,/* comment */ȡkey2/* comment */:/* comment */\"foo2\"/* comment */,/* comment */\u{0221}key3/* comment */:/* comment */-1/* comment */,/* comment */'key4'/* comment */:/* comment */[/* comment */]/* comment */,/* comment */\"key5\"/* comment */:/* comment */{/* comment */}/* comment */,/* comment */}/* comment */", CONFIG_JSON5);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("\u{0221}key2"), true);
    try std.testing.expect(jsonResult.get("\u{0221}key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("\u{0221}key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("ȡkey3"), true);
    try std.testing.expect(jsonResult.get("ȡkey3") == .integer);
    try std.testing.expectEqual(jsonResult.get("ȡkey3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "JSON5.7: parseObject ignores single-line comments" {
    const allocator = std.testing.allocator;

    _, const jsonResult = try parseObject(allocator, "// comment \n{// comment \nkey1// comment \n:// comment \n\"foo\"// comment \n,// comment \nȡkey2// comment \n:// comment \n\"foo2\"// comment \n,// comment \n\u{0221}key3// comment \n:// comment \n-1// comment \n,// comment \n'key4'// comment \n:// comment \n[// comment \n]// comment \n,// comment \n\"key5\"// comment \n:// comment \n{// comment \n}// comment \n,// comment \n}// comment \n", CONFIG_JSON5);
    defer jsonResult.deinit(allocator);
    try std.testing.expect(jsonResult == .object);

    try std.testing.expectEqual(jsonResult.asObject().contains("key1"), true);
    try std.testing.expect(jsonResult.get("key1") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("key1").asString(), "foo"));

    try std.testing.expectEqual(jsonResult.asObject().contains("\u{0221}key2"), true);
    try std.testing.expect(jsonResult.get("\u{0221}key2") == .string);
    try std.testing.expect(std.mem.eql(u8, jsonResult.get("\u{0221}key2").asString(), "foo2"));

    try std.testing.expectEqual(jsonResult.asObject().contains("ȡkey3"), true);
    try std.testing.expect(jsonResult.get("ȡkey3") == .integer);
    try std.testing.expectEqual(jsonResult.get("ȡkey3").asInteger(), -1);

    try std.testing.expectEqual(jsonResult.asObject().contains("key4"), true);
    try std.testing.expect(jsonResult.get("key4") == .array);
    try std.testing.expectEqual(jsonResult.get("key4").len(), 0);

    try std.testing.expectEqual(jsonResult.asObject().contains("key5"), true);
    try std.testing.expect(jsonResult.get("key5") == .object);
    try std.testing.expectEqual(jsonResult.get("key5").len(), 0);
}

test "JSON5.4 parseNumber ignores multi-line comments" {
    try expectParseNumberToParseNumber(0.0, "/* comment */0.0/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.0, "/* comment */-0.0/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.0, "/* comment */+0.0/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "/* comment */0.1/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "/* comment */.1/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "/* comment */+.1/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0.1, "/* comment */-.1/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "/* comment */+0.1/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0.1, "/* comment */-0.1/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(100.0, "/* comment */100.0/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-100.0, "/* comment */-100.0/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(std.math.inf(f64), "/* comment */Infinity/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-std.math.inf(f64), "/* comment */-Infinity/* comment */", CONFIG_JSON5);
    try expectParseNumberToParseNumber(std.math.inf(f64), "/* comment */+Infinity/* comment */", CONFIG_JSON5);
}

test "JSON5.4 parseNumber ignores single-line comments" {
    try expectParseNumberToParseNumber(0.0, "// comment\n0.0\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.0, "// comment\n-0.0\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.0, "// comment\n+0.0\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "// comment\n0.1\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "// comment\n.1\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "// comment\n+.1\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0.1, "// comment\n-.1\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(0.1, "// comment\n+0.1\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-0.1, "// comment\n-0.1\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(100.0, "// comment\n100.0\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-100.0, "// comment\n-100.0\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(std.math.inf(f64), "// comment\nInfinity\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(-std.math.inf(f64), "// comment\n-Infinity\n// comment", CONFIG_JSON5);
    try expectParseNumberToParseNumber(std.math.inf(f64), "// comment\n+Infinity\n// comment", CONFIG_JSON5);
}

test "README.md simple test" {
    const allocator = std.testing.allocator;

    const buffer =
        \\{
        \\  "foo": [
        \\    null,
        \\    true,
        \\    false,
        \\    "bar",
        \\    {
        \\      "baz": -13e+37
        \\    }
        \\  ]
        \\}
    ;
    var root = try parse(allocator, buffer);
    const bazObj = root.value.get("foo").get(4);

    try bazObj.serialize(std.io.getStdErr().writer(), .SPACES_2, 0);

    try std.testing.expectEqual(bazObj.get("baz").asFloat(), -13e+37);

    defer root.deinit();
}

test "README.md simple test json5" {
    const allocator = std.testing.allocator;

    const buffer =
        \\{
        \\  foo: [
        \\    /* Some
        \\     * multi-line comment
        \\     */ null,
        \\    true,
        \\    false,
        \\    "bar",
        \\    // Single line comment
        \\    {
        \\      baz: -13e+37,
        \\      'nan': NaN,
        \\      inf: +Infinity,
        \\    },
        \\  ],
        \\}
    ;
    var root = try parseJson5(allocator, buffer);
    const bazObj = root.value.get("foo").get(4);

    try bazObj.serialize(std.io.getStdErr().writer(), .SPACES_2, 0);

    try std.testing.expectEqual(bazObj.get("baz").asFloat(), -13e+37);

    defer root.deinit();
}

// TODO: For stream implementation
test "README.md simple test with stream source" {
    const allocator = std.testing.allocator;

    const source =
        \\{
        \\  foo: [
        \\    /* Some
        \\     * multi-line comment
        \\     */ null,
        \\    true,
        \\    false,
        \\    "bar",
        \\    // Single line comment
        \\    {
        \\      baz: -13e+37,
        \\      'nan': NaN,
        \\      inf: +Infinity,
        \\    },
        \\  ],
        \\}
    ;
    var root = try parseJson5(allocator, source);
    defer root.deinit();

    const bazObj = root.value.get("foo").get(4);

    try bazObj.serialize(std.io.getStdErr().writer(), .SPACES_2, 0);

    try std.testing.expectEqual(bazObj.get("baz").asFloat(), -13e+37);
}

test "Underread Literal" {
    const allocator = std.testing.allocator;

    {
        const value = parseValue(allocator, "tru", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "t", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "fals", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "f", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
}

test "Match Literal" {
    const allocator = std.testing.allocator;

    {
        const value = parseValue(allocator, "truee", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "truex", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "falsee", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "falsex", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "nulll", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
    {
        const value = parseValue(allocator, "nullx", CONFIG_RFC8259);
        try std.testing.expectEqual(error.ParseValueError, value);
    }
}

test "Escaped Strings" {
    const allocator = std.testing.allocator;

    {
        _, const value = try parseStringWithTerminal(allocator, "\"\\u0020\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
        defer value.deinit(allocator);
        try std.testing.expect(value == .string);
        try std.testing.expectEqualStrings(" ", value.asString());
    }

    {
        _, const value = try parseStringWithTerminal(allocator, "\"\\u0009\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
        defer value.deinit(allocator);
        try std.testing.expect(value == .string);
        try std.testing.expectEqualStrings(&[_]u8{9}, value.asString());
    }

    {
        _, const value = try parseStringWithTerminal(allocator, "\"\\b\\b\\b\"", CONFIG_RFC8259, TOKEN_DOUBLE_QUOTE);
        defer value.deinit(allocator);
        try std.testing.expect(value == .string);
        try std.testing.expectEqualStrings(&[_]u8{ 8, 8, 8 }, value.asString());
    }
}

test "README.md simple test from file" {
    const allocator = std.testing.allocator;

    const file = try std.fs.cwd().openFile("testFiles/some.json", .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
    defer allocator.free(content);

    var root = try parse(allocator, content);
    defer root.deinit();

    const bazObj = root.value.get("foo").get(4);

    try bazObj.serialize(std.io.getStdErr().writer(), .SPACES_2, 0);

    try std.testing.expectEqual(bazObj.get("baz").asFloat(), -13e+37);
}

test "Custom Json Insert - Array" {
    const allocator = std.testing.allocator;

    const ptr = try allocator.create(std.ArrayList(JsonValue));
    ptr.* = std.ArrayList(JsonValue).init(allocator);
    var root = JsonRoot.init(allocator, JsonValue{
        .array = ptr,
    });
    defer root.deinit();

    _ = try root.value.append(.{ .static_string = "foo" });
    _ = try root.value.append(.{ .static_string = "foo" });
    _ = try root.value.append(.{ .static_string = "foo" });
    _ = try root.value.append(.{ .static_string = "foo" });
    _ = try root.value.append(.{ .static_string = "foo" });

    try root.value.serialize(std.io.getStdErr().writer(), .SPACES_2, 0);
}

test "Custom Json Insert - Object" {
    const allocator = std.testing.allocator;

    const ptr = try allocator.create(std.StringArrayHashMap(JsonValue));
    ptr.* = std.StringArrayHashMap(JsonValue).init(allocator);
    var root = JsonRoot.init(allocator, JsonValue{
        .object = ptr,
    });
    defer root.deinit();

    try root.value.set("test", .{ .static_string = "foo" });
    try root.value.set("test", .{ .static_string = "foo" });
    try root.value.set("test", .{ .static_string = "foo" });
    try root.value.set("test", .{ .static_string = "foo" });
    try root.value.set("test", .{ .static_string = "foo" });
    const value = try root.value.setWith("test", .{ .static_string = "foo" });

    try std.testing.expect(value == .static_string);
    try std.testing.expectEqualStrings("foo", value.static_string);
    try std.testing.expectEqualStrings("foo", value.asString());
    try std.testing.expectEqualStrings("foo", value.stringOrNull().?);

    try root.value.serialize(std.io.getStdErr().writer(), .SPACES_2, 0);
}

// Check whether tests are executed.
//test{try std.testing.expect(false);}
