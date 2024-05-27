package main

import "fmt"

func (expr intValueExpression) eval(_ map[varName]int) (int, error) {
	return expr.value, nil
}

func (expr variableExpression) eval(vars map[varName]int) (int, error) {
	if val, ok := vars[expr.name]; ok {
		return val, nil
	}
	return 0, fmt.Errorf("undefined variable: %s", expr.name)
}

func (expr addExpression) eval(vars map[varName]int) (int, error) {
	l, err := expr.left.eval(vars)
	if err != nil {
		return 0, err
	}
	r, err := expr.right.eval(vars)
	if err != nil {
		return 0, err
	}
	return l + r, nil
}

func (expr subExpression) eval(vars map[varName]int) (int, error) {
	l, err := expr.left.eval(vars)
	if err != nil {
		return 0, err
	}
	r, err := expr.right.eval(vars)
	if err != nil {
		return 0, err
	}
	return l - r, nil
}

func (expr boolValueExpression) eval(_ map[varName]int) (bool, error) {
	return expr.value, nil
}

func (expr eqExpression) eval(vars map[varName]int) (bool, error) {
	l, err := expr.left.eval(vars)
	if err != nil {
		return false, err
	}
	r, err := expr.right.eval(vars)
	if err != nil {
		return false, err
	}
	return l == r, nil
}

func (expr ltExpression) eval(vars map[varName]int) (bool, error) {
	l, err := expr.left.eval(vars)
	if err != nil {
		return false, err
	}
	r, err := expr.right.eval(vars)
	if err != nil {
		return false, err
	}
	return l < r, nil
}

func (expr notExpression) eval(vars map[varName]int) (bool, error) {
	val, err := expr.expression.eval(vars)
	if err != nil {
		return false, err
	}
	return !val, nil
}

func (expr orExpression) eval(vars map[varName]int) (bool, error) {
	l, err := expr.left.eval(vars)
	if err != nil {
		return false, err
	}
	r, err := expr.right.eval(vars)
	if err != nil {
		return false, err
	}
	return l || r, nil
}
