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

// Greenspuns 10th rule?
struct Value
{
    int64_t _repr;
    enum Type { Pair, Number, Symbol, Ref };
    inline Type type() const { return (Type)(_repr&3); }

    Value()                     : _repr(0)                                  {}
    Value(int number)           : _repr((number<<2)|Number)                 {}
    Value(int64_t number)       : _repr((number<<2)|Number)                 {}
    Value(char symbol)          : _repr((symbol<<2)|Symbol)                 {}
    Value(Value car, Value cdr) : _repr(alloc(car,cdr))                     {}
    Value(Value* ref)           : _repr(reinterpret_cast<int64_t>(ref)|Ref) {}

    inline bool operator == (const Value& rhs) const
    {
        if (type() == Ref)
        {
            Value* ref = reinterpret_cast<Value*>(_repr&~3);
            *ref = rhs;
            return true;
        }
        if (_repr == rhs._repr) return true;
        if (type() != Pair or rhs.type() != Pair) return false;
        if (_repr == 0 or rhs._repr == 0) return false;
        return car() == rhs.car() and cdr() == rhs.cdr();
    }

    inline bool operator != (const Value& rhs) const { return !operator == (rhs); } 

    inline Value car() const { return pair()->first; }
    inline Value cdr() const { return pair()->second; }
    inline int64_t number() const { return _repr >> 2; }

    static const Value NIL;
    static void clear_heap() { heap.clear(); }

private:
    static std::list<std::pair<Value,Value>> heap;
    static int64_t alloc(Value car, Value cdr)
    {
        heap.push_front(std::make_pair(car, cdr));
        return reinterpret_cast<int64_t>(&heap.front());
    }
    std::pair<Value,Value>* pair() const
    {
        return reinterpret_cast<std::pair<Value,Value>*>(_repr);
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
int64_t parse_integer(Iterator& begin)
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
    switch (v.type())
    {
    case Value::Number:
        {
            std::ostringstream o;
            o << v.number();
            return o.str();
        }
    case Value::Symbol:
        switch (char(v.number()))
        {
        case 's': return "sin";
        case 'c': return "cos";
        case 't': return "tan";
        case 'e': return "exp";
        case 'l': return "ln";
        default:  return std::string{char(v.number())};
        }
    case Value::Pair:
        {
            std::string result = "(";
            for (Value i = v; i != Value::NIL; i = i.cdr())
            {
                if (i != v) result += " ";
                result += to_s(i.car());
            }
            return result + ")";
        }
    case Value::Ref:
        return "&";
    }
}

Value simplify(Value expr)
{
    Value a, b;
    if (L('+', 0, &a) == expr) return simplify(a);
    if (L('+', &a, 0) == expr) return simplify(a);
    if (L('+', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type() == Value::Number and b.type() == Value::Number)
            return Value{a.number() + b.number()};
        return L('+', a, b);
    }
    if (L('-', &a, 0) == expr) return simplify(a);
    if (L('-', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type() == Value::Number and b.type() == Value::Number)
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
        if (a.type() == Value::Number and b.type() == Value::Number)
            return Value{a.number() * b.number()};
        return L('*', a, b);
    }
    if (L('/', &a, 1) == expr) return simplify(a);
    if (L('/', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type() == Value::Number and b.type() == Value::Number)
            return Value{a.number() / b.number()};
        return L('/', a, b);
    }
    if (L('^', &a, 1) == expr) return simplify(a);
    if (L('^', &a, 0) == expr) return {1};
    if (L('^', &a, &b) == expr)
    {
        a = simplify(a);
        b = simplify(b);
        if (a.type() == Value::Number and b.type() == Value::Number)
            return Value{int64_t(pow(a.number(), b.number()))};
        return L('^', a, b);
    }
    return expr;
}

Value differentiate(Value expr)
{
    Value a, b;
    if (expr == Value{'x'})           return {1};
    if (expr.type() == Value::Number) return {0};
    if (L('^', &a, &b) == expr)       return L('^', L('*', a, b), L('-', b, 1));
    if (L('+', &a, &b) == expr)       return L('+', differentiate(a), differentiate(b));
    if (L('-', &a, &b) == expr)       return L('-', differentiate(a), differentiate(b));
    if (L('*', &a, &b) == expr)       return L('+', L('*', differentiate(a), b), L('*', a, differentiate(b)));
    if (L('/', &a, &b) == expr)       return L('/', L('-', L('*', differentiate(a), b), L('*', a, differentiate(b))),
                                                    L('^', b, 2));
    //assert(false);
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
