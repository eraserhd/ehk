#include <cassert>
#include <cctype>
#include <cmath>
#include <cstdint>
#include <iostream>
#include <iterator>
#include <memory>
#include <sstream>
#include <string>
#include <list>

typedef double Number;

// Greenspuns 10th rule?
struct Value
{
    enum class Type { Pair, Number, Symbol, Ref };

    Type type;

    union
    {
        char _symbol;
        Number _number;
        std::pair<Value,Value>* _pair;
        Value* _ref;
    };

    Value()                     : type(Type::Pair),   _pair(nullptr)        {}
    Value(int number)           : type(Type::Number), _number(number)       {}
    Value(Number number)        : type(Type::Number), _number(number)       {}
    Value(char symbol)          : type(Type::Symbol), _symbol(symbol)       {}
    Value(Value car, Value cdr) : type(Type::Pair),   _pair(alloc(car,cdr)) {}
    Value(Value* ref)           : type(Type::Ref),    _ref(ref)             {}

    inline bool operator == (const Value& rhs) const
    {
        if (type != Type::Ref and type != rhs.type) return false;
        switch (type)
        {
        case Type::Pair:
            if (_pair == rhs._pair) return true;
            if (_pair == nullptr or rhs._pair == nullptr) return false;
            return _pair->first == rhs._pair->first and _pair->second == rhs._pair->second;
        case Type::Number:
            return _number == rhs._number;
        case Type::Symbol:
            return _symbol == rhs._symbol;
        case Type::Ref:
            *_ref = rhs;
            return true;
        }
    }

    inline bool operator != (const Value& rhs) const { return !operator == (rhs); } 

    inline Value car() const { return _pair->first; }
    inline Value cdr() const { return _pair->second; }
    inline Number number() const { return _number; }
    inline char symbol() const { return _symbol; }

    static const Value NIL;
    static void clear_heap() { heap.clear(); }

private:
    static std::list<std::pair<Value,Value>> heap;
    static std::pair<Value,Value>* alloc(Value car, Value cdr)
    {
        heap.push_front(std::make_pair(car, cdr));
        return &heap.front();
    }
};

Value reverse(Value l)
{
    Value result = Value::NIL;
    for (; l != Value::NIL; l = l.cdr())
        result = Value{l.car(), result};
    return result;
}

template<typename Iterator>
Number parse_integer(Iterator& begin)
{
    int64_t result = 0;
    while (isdigit(*begin))
        result = 10*result + (*begin++ - '0');
    return result;
}

template<typename Iterator>
Value parse_rec(Iterator& begin)
{
    Value result;
    switch (*begin++)
    {
    case '-':
        if (isdigit(*begin))
            result = Value{-parse_integer(begin)};
        else
            result = Value{'-'};
        break;
    case '0' ... '9':
        --begin;
        result = Value{parse_integer(begin)};
        break;
    case '(':
        while (*begin != ')')
            result = Value{parse_rec(begin),result};
        ++begin;
        result = reverse(result);
        break;
    default:
        result = Value{*(begin-1)};
        while (*begin and isalpha(*begin))
            ++begin;
        break;
    }
    while (*begin and isspace(*begin))
        ++begin;
    return result;
}

template<typename T>
Value parse(T const& what)
{
    auto it = std::cbegin(what);
    return parse_rec(it);
}

std::list<std::pair<Value,Value>> Value::heap;
const Value Value::NIL{};

inline Value L() { return Value::NIL; }
template<typename Arg1, typename ...Args>
inline Value L(Arg1 arg1, Args... args) { return Value{arg1, L(args...)}; }

std::string to_s(Value v)
{
    switch (v.type)
    {
    case Value::Type::Number:
        {
            std::ostringstream o;
            o << v.number();
            return o.str();
        }
    case Value::Type::Symbol:
        switch (char(v.number()))
        {
        case 's': return "sin";
        case 'c': return "cos";
        case 't': return "tan";
        case 'e': return "exp";
        case 'l': return "ln";
        default:  return std::string{char(v.number())};
        }
    case Value::Type::Pair:
        {
            std::string result = "(";
            for (Value i = v; i != Value::NIL; i = i.cdr())
            {
                if (i != v) result += " ";
                result += to_s(i.car());
            }
            return result + ")";
        }
    case Value::Type::Ref:
        return "&";
    }
}

Value simplify(Value expr);

Value simplify(Value expr)
{
    Value a, b;
    if (L('+', 0, &a) == expr) return simplify(a);
    if (L('+', &a, 0) == expr) return simplify(a);
    if (L('+', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type == Value::Type::Number and b.type == Value::Type::Number)
            return Value{a.number() + b.number()};
        return L('+', a, b);
    }
    if (L('-', &a, 0) == expr) return simplify(a);
    if (L('-', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type == Value::Type::Number and b.type == Value::Type::Number)
            return Value{a.number() - b.number()};
        return L('-', a, b);
    }
    if (L('*', 0, &a) == expr) return {0};
    if (L('*', &a, 0) == expr) return {0};
    if (L('*', 1, &a) == expr) return simplify(a);
    if (L('*', &a, 1) == expr) return simplify(a);
    if (L('*', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type == Value::Type::Number and b.type == Value::Type::Number)
            return Value{a.number() * b.number()};
        return L('*', a, b);
    }
    if (L('/', &a, 1) == expr) return simplify(a);
    if (L('/', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type == Value::Type::Number and b.type == Value::Type::Number)
            return Value{a.number() / b.number()};
        return L('/', a, b);
    }
    if (L('^', &a, 1) == expr) return simplify(a);
    if (L('^', &a, 0) == expr) return {1};
    if (L('^', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type == Value::Type::Number and b.type == Value::Type::Number)
            return Value{Number(pow(a.number(), b.number()))};
        return L('^', a, b);
    }
    return expr;
}

Value differentiate(Value expr)
{
    Value a, b;
    if (expr == Value{'x'})           return {1};
    if (expr.type == Value::Type::Number) return {0};
    if (L('^', &a, &b) == expr)       return L('^', L('*', a, b), L('-', b, 1));
    if (L('+', &a, &b) == expr)       return L('+', differentiate(a), differentiate(b));
    if (L('-', &a, &b) == expr)       return L('-', differentiate(a), differentiate(b));
    if (L('*', &a, &b) == expr)       return L('+', L('*', differentiate(a), b), L('*', a, differentiate(b)));
    if (L('/', &a, &b) == expr)       return L('/', L('-', L('*', differentiate(a), b), L('*', a, differentiate(b))),
                                                    L('^', b, 2));
    // need chain rule
    if (L('s', 'x') == expr) return L('c', 'x');
    if (L('c', 'x') == expr) return L('-', 0, L('s', 'x'));
    if (L('t', 'x') == expr) return L('/', 1, L('^', L('c', 'x'), 2));
    if (L('e', 'x') == expr) return L('e', 'x');
    if (L('l', 'x') == expr) return L('/', 1, 'x');
    return expr;
}

std::string diff(const std::string& s)
{
    return to_s(simplify(differentiate(simplify(parse(s)))));
}

#ifdef TEST
int main(int argc, const char *argv[])
{
    // parse
    assert(Value{42} == parse("42"));
    assert(Value{'s'} == parse("sin"));
    assert(Value{-42} == parse("-42"));
    assert(Value::NIL == parse("()"));
    assert(L('+', 'x', 0) == parse("(+  x 0 )"));
    assert(L('+', L('*', 1, 0), 7) == parse("(+(* 1 0) 7)"));

    // matching
    Value a;
    assert(Value(&a) == parse("42"));

    // simplification
    assert(parse("42") == simplify(parse("42")));
    assert(parse("42") == simplify(parse("(+ 11 31)")));
}
#endif
