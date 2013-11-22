package Functions;

/**
 * Declaration of functions in a static enum.
 * Parser will use this enum for the functions.
 */
enum Functions {
	/*
	 * Register your functions here!
	 */
	ADD(Add.class),
	POW(Power.class);
	
	/*
	 *  Functions.valueOf("ADD") => ADD
	 *  ADD.toString() => "ADD"
	 */
	
	Class<? extends Function> c;
	
	Functions(Class <? extends Function > f)
	{
		c = f;
	}
	
	Function create(Function... arguments) throws InstantiationException, IllegalAccessException
	{
		Function f = c.newInstance();
		f.arguments = arguments;
		return f;
	}
}