import java.util.Arrays;
import java.util.LinkedList;
import java.util.Optional;

/** Class that handles evaluation, composition, and
 * differentiation of lisp expression inputs of multiple
 * variables. Simplification and optimization only supported
 * for single-variable equations.
 * @author Joey Zhu
 */
public class Function {
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

    /** Initialize a function w.r.t. VARS, using LISP input. */
    public Function(String[] vars, String lisp) {
        _vars = vars;
        _root = parse(lisp);
    }

    public Function(String var, String lisp) {
        _vars = new String[] { var };
        _root = parse(lisp);
    }

    /** Initialize a polynomial function by conducting Lagrange
     * Interpolation on several POINTS. */
    public Function(String var, int[][] points) {
        //TODO: Handle product-power collapsing of variables
        //TODO: Recursively multiply out terms by distribution
        //TODO: Regroup coefficients from across tree branches
    }

    /** Initialize an empty function w.r.t. VARS. Make sure to use
     * setRoot if this constructor is used. */
    public Function(String[] vars) {
        _vars = vars;
    }

    /** Initialize a no-variable expression to be evaluated. Only used
     * when collapsing atomic expressions to simplify. */
    public Function(FNode root) {
        _root = root;
        _vars = new String[] {};
    }

    /** Creates a copy of the function. */
    public Function(Function f) {
        _vars = f.getVars();
        _root = parse(f.toString(false));
    }

    /** Return an expression tree of FNodes by parsing LISP. */
    public FNode parse(String lisp) {
        boolean reading = true;
        if (lisp.charAt(0) != OPEN) {
            return new FNode(lisp, null, null);
        }
        LinkedList<String> tokens = new LinkedList<String>();
        int i = 1;
        StringBuilder token = new StringBuilder();
        while (i < lisp.length()) {
            char c = lisp.charAt(i);
            if (c == SPACE) {
                if (reading) {
                    reading = false;
                    if (token.length() > 0) tokens.add(token.toString());
                    token = new StringBuilder();
                }
            } else {
                reading = true;
                if (c == CLOSE) {
                    if (token.length() > 0) tokens.add(token.toString());
                } else if (c == OPEN) {
                    int delim = 1;
                    i++;
                    StringBuilder subexp = new StringBuilder("(");
                    while (delim != 0) {
                        c = lisp.charAt(i);
                        if (c == OPEN) delim++;
                        else if (c == CLOSE) delim--;
                        subexp.append(c);
                        i++;
                    }
                    tokens.add(subexp.toString());
                } else {
                    token.append(c);
                }
            }
            i++;
        }
        //TODO: Implement reduction for add and mul tuples
        if (tokens.size() == 3) {
            return new FNode(tokens.get(0), parse(tokens.get(1)), parse(tokens.get(2)));
        } else if (tokens.size() == 2) {
            return new FNode(tokens.get(0), parse(tokens.get(1)), null);
        } else if (tokens.size() == 1) {
            return new FNode(tokens.get(0), null, null);
        } else {
            System.out.println("too many tokens");
            return null;
        }
    }

    /** Return the evaluation when setting values for corresponding
     * VARS this function was initialized with. */
    public double eval(double[] vars) throws Exception {
        return eval(_root, vars);
    }

    /** Recursively evaluate the expression described by ROOT with
     * the values VARS. */
    public double eval(FNode root, double[] vars) throws Exception {
        if (root.getRight() == null) {
            if (root.getLeft() == null) {
                int varIndex = index(root.getToken());
                if (varIndex == -1) {
                    return Double.parseDouble(root.getToken());
                } else {
                    return vars[varIndex];
                }
            } else {
                //Recursively run unary function
                if (root.getToken().equals(EXP_TOKEN)) {
                    return exp(eval(root.getLeft(), vars));
                } else if (root.getToken().equals(LOG_TOKEN)) {
                    return log(eval(root.getLeft(), vars));
                } else if (root.getToken().equals(SIN_TOKEN)) {
                    return sin(eval(root.getLeft(), vars));
                } else if (root.getToken().equals(COS_TOKEN)) {
                    return cos(eval(root.getLeft(), vars));
                } else if (root.getToken().equals(SQR_TOKEN)) {
                    return sqrt(eval(root.getLeft(), vars));
                } else throw new Exception("Invalid unary function");
            }
        } else {
            if (root.getToken().equals(ADD_TOKEN)) {
                return add(eval(root.getLeft(), vars), eval(root.getRight(), vars));
            } else if (root.getToken().equals(SUB_TOKEN)) {
                return sub(eval(root.getLeft(), vars), eval(root.getRight(), vars));
            } else if (root.getToken().equals(MUL_TOKEN)) {
                return mul(eval(root.getLeft(), vars), eval(root.getRight(), vars));
            } else if (root.getToken().equals(DIV_TOKEN)) {
                return div(eval(root.getLeft(), vars), eval(root.getRight(), vars));
            } else if (root.getToken().equals(POW_TOKEN)) {
                return pow(eval(root.getLeft(), vars), eval(root.getRight(), vars));
            } else throw new Exception("Invalid binary function");
        }
    }


    public void simplify() {
        _root.pushCoefs();
        _root.primarySimplify();
        _root.secondarySimplify();
        _root.primarySimplify();
    }

    /** Return this Function's partial derivative as another object
     *  w.r.t. variable X. */
    public static Function derivative(Function f, String x) {
        String[] newVars = new String[f.getVars().length];
        System.arraycopy(f.getVars(), 0, newVars, 0, f.getVars().length);
        Function func = new Function(newVars);
        func.setRoot(diff(f.getRoot(), x));
        func.getRoot().secondarySimplify();
        return func;
    }

    /** Substitute this function's variables with new TRANSFORMS. */
    public void transform(Function... transforms) {
        for (Function f : transforms) {
            if (!Arrays.equals(transforms[0].getVars(), f.getVars())) {
                return;
            }
        }
        transform(_root, transforms);
        _vars = transforms[0].getVars();
    }

    /** Overload to recursively propagate through the function's expression tree. */
    public void transform(FNode node, Function[] transforms) {
        if (node.getRight() != null) {
            transform(node.getRight(), transforms);
        }
        if (node.getLeft() != null) {
            transform(node.getLeft(), transforms);
        } else {
            int i = index(node.getToken());
            if (i != -1) {
                node.set(transforms[i].getRoot());
            }
        }
    }

    /** Recursively differentiate node F w.r.t. variable X and return the root
     * of the resulting expression tree. */
    private static FNode diff(FNode f, String x) {
        FNode g;
        if (f.getToken().equals(ADD_TOKEN)) {
            g = new FNode(ADD_TOKEN, diff(f.getLeft(), x), diff(f.getRight(), x));
        } else if (f.getToken().equals(SUB_TOKEN)) {
            g = new FNode(SUB_TOKEN, diff(f.getLeft(), x), diff(f.getRight(), x));
        } else if (f.getToken().equals(MUL_TOKEN)) {
            g = new FNode(ADD_TOKEN,
                    new FNode(MUL_TOKEN, diff(f.getLeft(), x), f.getRight()),
                    new FNode(MUL_TOKEN, diff(f.getRight(), x), f.getLeft()));
        } else if (f.getToken().equals(DIV_TOKEN)) {
            g = new FNode(DIV_TOKEN,
                    new FNode(SUB_TOKEN,
                            new FNode(MUL_TOKEN, diff(f.getLeft(), x), f.getRight()),
                            new FNode(MUL_TOKEN, diff(f.getRight(), x), f.getLeft())),
                    new FNode(POW_TOKEN, f.getRight(), new FNode("2")));
        } else if (f.getToken().equals(POW_TOKEN)) {
            g = new FNode(MUL_TOKEN,
                    diff(f.getLeft(), x),
                    new FNode(MUL_TOKEN,
                            f.getRight(),
                            new FNode(POW_TOKEN,
                                    f.getLeft(),
                                    new FNode(SUB_TOKEN, f.getRight(), new FNode("1")))));
        } else if (f.getToken().equals(EXP_TOKEN)) {
            g = new FNode(MUL_TOKEN,
                    diff(f.getLeft(), x),
                    new FNode(EXP_TOKEN, f.getLeft(), null));
        } else if (f.getToken().equals(LOG_TOKEN)) {
            g = new FNode(DIV_TOKEN,
                    diff(f.getLeft(), x),
                    f.getLeft());
        } else if (f.getToken().equals(SIN_TOKEN)) {
            g = new FNode(MUL_TOKEN,
                    diff(f.getLeft(), x),
                    new FNode(COS_TOKEN, f.getLeft(), null));
        } else if (f.getToken().equals(COS_TOKEN)) {
            g = new FNode(MUL_TOKEN,
                    diff(f.getLeft(), x),
                    new FNode(MUL_TOKEN,
                            new FNode("-1"),
                            new FNode(SIN_TOKEN, f.getLeft(), null)));
        } else if (f.getToken().equals(SQR_TOKEN)) {
            g = new FNode(DIV_TOKEN,
                    new FNode(MUL_TOKEN,
                            new FNode("0.5"),
                            diff(f.getLeft(), x)),
                    f);
            //TODO: Fix sqrt simplification
        } else if (f.getToken().equals(x)) {
            g = new FNode("1");
        } else {
            g = new FNode("0");
        }
        if (g.isBinary()) g.pushCoefs();
        g.primarySimplify();
        return g;
    }

    @Override
    public String toString() {
        return toString(false);
    }

    /** Show function's variables if DISPLAYVARS, else just output its lisp repr. */
    public String toString(boolean displayVars) {
        if (displayVars) {
            StringBuilder display = new StringBuilder("(");
            for (int i = 0; i < _vars.length; i++) {
                display.append(_vars[i]);
                if (i != _vars.length - 1) {
                    display.append(", ");
                }
            }
            display.append("): ");
            return display + _root.toString();
        } else {
            return _root.toString();
        }

    }

    public String toString(String funcName) {
        StringBuilder display = new StringBuilder(funcName + "(");
        for (int i = 0; i < _vars.length; i++) {
            display.append(_vars[i]);
            if (i != _vars.length - 1) {
                display.append(", ");
            }
        }
        display.append("): ");
        return display + _root.toString();
    }

    /** Return the variables this Function was instantiated with. */
    public String[] getVars() {
        return _vars;
    }

    /** Return the root of this Function's corresponding expression tree. */
    public FNode getRoot() {
        return _root;
    }

    /** Set this function's expression tree to ROOT. */
    private void setRoot(FNode root) {
        _root = root;
    }

    /** Return the array index of VAR of this function. */
    public int index(String var) {
        for (int i = 0; i < _vars.length; i++) {
            if (_vars[i].equals(var)) return i;
        }
        return -1;
    }

    /** The input variables which this Function takes. */
    private String[] _vars;
    private double[] domain;
    /** The root of this Function's expression tree. */
    private FNode _root;

    /** All binary operations that the Function can support. */
    private static double add(double a, double b) {
        return a + b;
    }
    private static double sub(double a, double b) {
        return a - b;
    }
    private static double mul(double a, double b) {
        return a * b;
    }
    private static double div(double a, double b) throws Exception {
        if (b == 0) throw new Exception("Divide by zero");
        else return (a / b);
    }
    private static double pow(double a, double b) {
        return Math.pow(a, b);
    }

    /** All unary operations that the Function can support. */
    private static double exp(double a) {
        return (Math.exp(a));
    }
    private static double sin(double a) {
        return (Math.sin(a));
    }
    private static double cos(double a) {
        return (Math.cos(a));
    }
    private static double log(double a) throws Exception {
        if (a <= 0) throw new Exception("Log by zero");
        return (Math.log(a));
    }
    private static double sqrt(double a) throws Exception {
        if (a <= 0) throw new Exception("Sqrt by zero");
        return (Math.sqrt(a));
    }

    /** Inner class that handles the Function's expression tree. */

}
