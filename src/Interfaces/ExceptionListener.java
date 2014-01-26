package Interfaces;

/**
 * This is the interface for classes that are able to delegate Exceptions upstream.
 * @author Jan-Willem Gmelig Meyling
 */
public interface ExceptionListener {
	
	/**
	 * When a Exception occurs, delegate the Exception upstream so that the GUI can supply appropiate information.
	 * @param e
	 */
	void onException(Exception e);
}