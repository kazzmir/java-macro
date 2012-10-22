public class Token{
    protected int x;
	public Token(Token parent){
		this(0, parent, null);

        macro foobar(){ x:expression }{
          syntax(x * 2)
        }

        foobar 5;
	}
}
