package main

type system struct {
	variables Variables
	processes []process
}

func System(vars Variables, procs ...process) system {
	return system{
		variables: vars,
		processes: procs,
	}
}

type (
	varName   string
	Variables map[varName]int
)

type procName string

type process struct {
	name       procName
	statements []statement
}

func Process(name procName, stmts ...statement) process {
	return process{
		name:       name,
		statements: stmts,
	}
}

type intExpression interface {
	eval(map[varName]int) (int, error)
}

var (
	_ intExpression = intValueExpression{}
	_ intExpression = variableExpression{}
	_ intExpression = addExpression{}
	_ intExpression = subExpression{}
)

type intValueExpression struct {
	value int
}

func Int(val int) intValueExpression {
	return intValueExpression{
		value: val,
	}
}

type variableExpression struct {
	name varName
}

func Var(name varName) variableExpression {
	return variableExpression{
		name: name,
	}
}

type addExpression struct {
	left  intExpression
	right intExpression
}

func Add(l intExpression, r intExpression) addExpression {
	return addExpression{
		left:  l,
		right: r,
	}
}

type subExpression struct {
	left  intExpression
	right intExpression
}

func Sub(l intExpression, r intExpression) subExpression {
	return subExpression{
		left:  l,
		right: r,
	}
}

type boolExpression interface {
	eval(map[varName]int) (bool, error)
}

var (
	_ boolExpression = boolValueExpression{}
	_ boolExpression = eqExpression{}
	_ boolExpression = ltExpression{}
	_ boolExpression = notExpression{}
	_ boolExpression = orExpression{}
)

type boolValueExpression struct {
	value bool
}

func True() boolValueExpression {
	return boolValueExpression{
		value: true,
	}
}

type eqExpression struct {
	left  intExpression
	right intExpression
}

func Eq(l intExpression, r intExpression) eqExpression {
	return eqExpression{
		left:  l,
		right: r,
	}
}

type ltExpression struct {
	left  intExpression
	right intExpression
}

func Lt(l intExpression, r intExpression) ltExpression {
	return ltExpression{
		left:  l,
		right: r,
	}
}

type notExpression struct {
	expression boolExpression
}

func Not(bexpr boolExpression) notExpression {
	return notExpression{
		expression: bexpr,
	}
}

type orExpression struct {
	left  boolExpression
	right boolExpression
}

func Or(l boolExpression, r boolExpression) orExpression {
	return orExpression{
		left:  l,
		right: r,
	}
}

func Neq(l intExpression, r intExpression) notExpression {
	return Not(Eq(l, r))
}

func Le(l intExpression, r intExpression) orExpression {
	return Or(Lt(l, r), Eq(l, r))
}

func Gt(l intExpression, r intExpression) notExpression {
	return Not(Le(l, r))
}

func Ge(l intExpression, r intExpression) notExpression {
	return Not(Lt(l, r))
}

func And(l boolExpression, r boolExpression) notExpression {
	return Not(Or(Not(l), Not(r)))
}
