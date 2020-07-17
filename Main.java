public class Main {
    public static void main(String[] args) {
        Function f = new Function("x", "(* (pow x 3) (* (pow x 4) (* (pow x 5) (pow x 6))))");
        f.simplify();
        System.out.println(f);
        //Function fp = new Function(f);

        //Function xprime = new Function("t", "(pow t 2)");
        //Function yprime = new Function("t", "(pow t 3)");
        //fp.transform()

        //Display.display( new Function[] { f, fp } );

        //System.out.println(f.toString(true));
        //System.out.println(fp.toString(true));
        //System.out.println(fpp);
        //System.out.println("");
    }
}
