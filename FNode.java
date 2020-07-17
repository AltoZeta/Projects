import java.util.Optional;
import java.util.ArrayList;

/** A class for storing and handling the expression tree representing
 * a function. Includes methods for partial differentiation and
 * REPL-tree optimization.
 * @author Joey Zhu
 */
public class FNode {
    /** Standard lisp tokens for function input. */
    private static final char OPEN = '(';
    private static final char CLOSE = ')';
    private static final char SPACE = ' ';
    private static final String ADD_TOKEN = "+";
    private static final String SUB_TOKEN = "-";
    private static final String MUL_TOKEN = "*";
    private static final String DIV_TOKEN = "/";
    private static final String POW_TOKEN = "pow";
    private static final String EXP_TOKEN = "e^";
    private static final String LOG_TOKEN = "ln";
    private static final String SIN_TOKEN = "sin";
    private static final String COS_TOKEN = "cos";
    private static final String SQR_TOKEN = "sqrt";


    /** Construction of a unary or binary operation TOKEN,
     * with LEFT as first input and RIGHT as second input. If
     * unary function, RIGHT = null. */
    public FNode(String token, FNode left, FNode right) {
        _token = token;
        _left = left;
        _right = right;

        if (isBinary() && !_token.equals(POW_TOKEN)) pushCoefs();
        primarySimplify();
    }

    /** Construct a leaf node of a single coefficient or variable. */
    public FNode(String token) {
        _token = token;
        _left = null;
        _right = null;
    }

    /** Reset this FNode to NODE. */
    public void set(FNode node) {
        _token = node.getToken();
        _left = node.getLeft();
        _right = node.getRight();
    }

    /** Send coefficients to the left side of the tree for simplification. */
    public void pushCoefs() {
        Optional<Double> rightNum = Optional.empty();
        if (_right != null && _right.isAtom()) {
            try {
                rightNum = Optional.of(Double.parseDouble(_right.getToken()));
            } catch (NumberFormatException e) { }
        }
        if (rightNum.isPresent()) {
            if (_token.equals(ADD_TOKEN) || _token.equals(MUL_TOKEN)) {
                FNode temp = _left;
                _left = _right;
                _right = temp;
            } else if (_token.equals(SUB_TOKEN)) {
                Double newNum = rightNum.get() * -1;
                FNode temp = new FNode(newNum.toString());
                _right = _left;
                _left = temp;
                _token = ADD_TOKEN;
            } else if (_token.equals(DIV_TOKEN)) {
                Double newNum = 1.0 / rightNum.get();
                FNode temp = new FNode(newNum.toString());
                _right = _left;
                _left = temp;
                _token = MUL_TOKEN;
            }
        }
    }

    /** Return a double parsed by the token, else return NULL. */
    public Optional<Double> extract(FNode node) {
        Optional<Double> num = Optional.empty();
        if (node.isAtom()) {
            try {
                num = Optional.of(Double.parseDouble(node.getToken()));
            } catch (NumberFormatException e) {
                num = Optional.empty();
            }
        }
        return num;
    }

    /** Collapse identity or coefficient-combining operations. */
    public void primarySimplify() {
        if (isAtom()) return;
        else if (isUnary()) {
            Optional<Double> leftNum = extract(_left);
            if (leftNum.isPresent()) {
                Function f = new Function(this);
                try {
                    Double num = f.eval(new double[]{ 0 });
                    _token = num.toString();
                    _left = null;
                    _right = null;
                } catch (Exception e) {
                    e.printStackTrace();
                }
            } return;
        }
        Optional<Double> leftNum = extract(_left);
        Optional<Double> rightNum = extract(_right);

        if (leftNum.isPresent() && rightNum.isPresent()) {
            Function f = new Function(this);
            try {
                Double num = f.eval(new double[]{ 0 });
                _token = num.toString();
                _left = null;
                _right = null;
            } catch (Exception e) {
                e.printStackTrace();
            }
        } else {
            if (_token.equals(ADD_TOKEN)) {
                if (leftNum.isPresent()) {
                    if (leftNum.get() == 0) {
                        _token = _right.getToken();
                        _left = _right.getLeft();
                        _right = _right.getRight();
                    }
                }
            } else if (_token.equals(MUL_TOKEN)) {
                if (leftNum.isPresent()) {
                    if (leftNum.get() == 0) {
                        _token = "0";
                        _left = null;
                        _right = null;
                    } else if (leftNum.get() == 1) {
                        _token = _right.getToken();
                        _left = _right.getLeft();
                        _right = _right.getRight();
                    } else if (_right.getToken().equals(MUL_TOKEN)) {
                        Optional<Double> rightLeft = Optional.empty();
                        if (_right.getLeft() != null && _right.getLeft().isAtom()) {
                            try {
                                rightLeft = Optional.of(Double.parseDouble(_right.getLeft().getToken()));
                                Double newCoef = leftNum.get() * rightLeft.get();
                                _left = new FNode(newCoef.toString());
                                _right = _right.getRight();
                            } catch (NumberFormatException e) { }
                        }
                    }
                }
            } else if (_token.equals(POW_TOKEN)) {
                if (rightNum.isPresent() && rightNum.get() == 1.0) {
                    _token = _left.getToken();
                    _right = _left.getRight();
                    _left = _left.getLeft();
                } else if (rightNum.isPresent() && rightNum.get() == 1.0) {
                    _token = _left.getToken();
                    _right = _left.getRight();
                    _left = _left.getLeft();
                    set(new FNode(DIV_TOKEN,
                            new FNode("1"),
                            this));
                }
            }
        }
    }

    /** Simplifies consecutive sums and products. Make sure functions have been
     * coefficient-shifted beforehand */
    public void secondarySimplify() {
        secondarySimplify(this);
    }

    /** Recursive overload on NODE. */
    public void secondarySimplify(FNode node) {
        ArrayList<FNode> factors = new ArrayList<>();
        ArrayList<Double> count = new ArrayList<>();
        factors.add(new FNode("1"));
        Double coef;
        if (node.isBinary()) {
            if (node.getToken().equals(ADD_TOKEN)) {
                count.add(0.0);
                coef = addFlatten(node, factors, count, true);
                count.set(0, coef);
                node.set(addReduce(factors, count, 0));
            } else if (node.getToken().equals(SUB_TOKEN)) {
                count.add(0.0);
                coef = addFlatten(node, factors, count, false);
                count.set(0, coef);
                node.set(addReduce(factors, count, 0));
            } else if (node.getToken().equals(MUL_TOKEN)) {
                count.add(1.0);
                coef = mulFlatten(node, factors, count, true);
                factors.set(0, new FNode(coef.toString()));
                node.set(mulReduce(factors, count, 0));
                //TODO: collapse terms of e^x
            } else if (node.getToken().equals(DIV_TOKEN)) {
                count.add(1.0);
                coef = mulFlatten(node, factors, count, false);
                factors.set(0, new FNode(coef.toString()));
                node.set(mulReduce(factors, count, 0));
            }
        } else if (node.isUnary()) {
            secondarySimplify(node.getLeft());
        }
    }

    /** Reduces a summation array and corresponding coefficients to a binary
     * expression tree. Mutually recursive with secondarySimplify and continues
     * to simplify each node. */
    public FNode addReduce(ArrayList<FNode> factors, ArrayList<Double> count, int index) {
        if (index == factors.size()) {
            return new FNode("0");
        } else {
            secondarySimplify(factors.get(index));
            return new FNode(ADD_TOKEN,
                    new FNode(MUL_TOKEN,
                            new FNode(count.get(index).toString()),
                            factors.get(index)),
                    addReduce(factors, count, index + 1));
        }
    }

    /** Reduces a product array and corresponding coefficients to a binary
     * expression tree. Mutually recursive with secondarySimplify and continues
     * to simplify each node. */
    public FNode mulReduce(ArrayList<FNode> factors, ArrayList<Double> count, int index) {
        if (index == factors.size()) {
            return new FNode("1");
        } else {
            secondarySimplify(factors.get(index));
            return new FNode(MUL_TOKEN,
                    new FNode(POW_TOKEN,
                            factors.get(index),
                            new FNode(count.get(index).toString())),
                    mulReduce(factors, count, index + 1));
        }
    }

    /** Collects all consecutively added elements in an expression tree. */
    public double addFlatten(FNode node, ArrayList<FNode> factors, ArrayList<Double> count, boolean mode) {
        if (node.getToken().equals(ADD_TOKEN)) {
            return addFlatten(node.getLeft(), factors, count, mode)
                    + addFlatten(node.getRight(), factors, count, mode);
        } else if (node.getToken().equals(SUB_TOKEN)) {
            return addFlatten(node.getLeft(), factors, count, mode)
                    - addFlatten(node.getRight(), factors, count, !mode);
        } else {
            double newCoef = mode ? 1 : -1;
            FNode factor = node;
            if (node.getToken().equals(MUL_TOKEN)) {
                Optional<Double> possibleCoef = extract(node.getLeft());
                if (possibleCoef.isPresent()) {
                    newCoef *= possibleCoef.get();
                    factor = node.getRight();
                }
            } else if (node.getToken().equals(DIV_TOKEN)) {
                Optional<Double> possibleCoef = extract(node.getLeft());
                if (possibleCoef.isPresent()) {
                    newCoef *= 1 / possibleCoef.get();
                    factor = node.getRight();
                }
            }
            Optional<Double> possibleCoef = extract(factor);
            if (possibleCoef.isPresent()) {
                return possibleCoef.get();
            } else {
                boolean commonFactor = false;
                for (int i = 0; i < factors.size(); i++) {
                    if (factor.equals(factors.get(i))) {
                        commonFactor = true;
                        count.set(i, count.get(i) + newCoef);
                    }
                }
                if (!commonFactor) {
                    factors.add(factor);
                    count.add(newCoef);
                }
                return 0;
            }
        }
    }

    /** Collects all consecutively multiplied elements in an expression tree. */
    public double mulFlatten(FNode node, ArrayList<FNode> factors, ArrayList<Double> count, boolean mode) {
        if (node.getToken().equals(MUL_TOKEN)) {
            return mulFlatten(node.getLeft(), factors, count, mode)
                    * mulFlatten(node.getRight(), factors, count, mode);
        } else if (node.getToken().equals(DIV_TOKEN)) {
            return mulFlatten(node.getLeft(), factors, count, mode)
                    / mulFlatten(node.getRight(), factors, count, !mode);
        } else {
            double newCoef = mode ? 1 : -1;
            FNode factor = node;
            if (node.getToken().equals(POW_TOKEN)) {
                Optional<Double> possibleCoef = extract(node.getRight());
                if (possibleCoef.isPresent()) {
                    newCoef *= possibleCoef.get();
                    factor = node.getLeft();
                }
            } else if (node.getToken().equals(LOG_TOKEN)) {

            }
            Optional<Double> possibleCoef = extract(node);
            if (possibleCoef.isPresent()) {
                return possibleCoef.get();
            } else {
                boolean commonFactor = false;
                for (int i = 0; i < factors.size(); i++) {
                    if (factor.equals(factors.get(i))) {
                        commonFactor = true;
                        count.set(i, count.get(i) + newCoef);
                    }
                }
                if (!commonFactor) {
                    factors.add(factor);
                    count.add(newCoef);
                }
                return 1;
            }
        }
    }

    @Override
    public String toString() {
        String s = "";
        if (_left != null || _right != null) s += "(";
        s += _token;
        if (_left != null) {
            s += " " + _left.toString();
        }
        if (_right != null) {
            s += " " + _right.toString();
        }
        if (_left != null || _right != null) s += ")";
        return s;
    }

    /** Return true iff this expression tree is equal to OTHER. */
    public boolean equals(FNode other) {
        if (isAtom() && other.isAtom()) {
            return (_token.equals(other.getToken()));
        } else if (isUnary() && other.isUnary()) {
            return (_token.equals(other.getToken()) && _left.equals(other.getLeft()));
        } else {
            return (_token.equals(other.getToken())
                    && _left.equals(other.getLeft())
                    && _right.equals(other.getRight()));
        }
    }

    public String getToken() {
        return _token;
    }
    public FNode getLeft() {
        return _left;
    }
    public FNode getRight() {
        return _right;
    }
    /** Return true iff this operand cannot be further reduced. */
    public boolean isAtom() {
        return (_left == null && _right == null);
    }
    /** Return true iff this operation is unary. */
    public boolean isUnary() {
        return (_left != null && _right == null);
    }
    /** Return true iff this operation is binary. */
    public boolean isBinary() {
        return (_left != null && _right != null);
    }

    /** The symbol corresponding to my operation. */
    private String _token;
    /** The first input that I take. */
    private FNode _left;
    /** The second input that I take, if I conduct a binary operation. */
    private FNode _right;
}
