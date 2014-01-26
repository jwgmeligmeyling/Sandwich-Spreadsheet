package Interfaces;

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import File.Sheet;

/**
 * The class for the {@code Workbook}. A workbook may contain several {@code Sheets}.
 * A workbook can be bound to a {@code File} in the system.
 * @author Jan-Willem Gmelig Meyling
 * @author Maarten Flikkema
 * @author Jim Hommes
 */
public interface Workbook {

	/**
	 * Append a sheet to this spreadsheet file.
	 * @param sheet
	 */
	void addSheet(Sheet sheet);
	
	/**
	 * @return get the sheets
	 */
	List<Sheet> getSheets();

	/**
	 * @return the file name
	 */
	String getName();

	/**
	 * @return the {@code File} for this {@code Workbook}, or {code null} if not saved.
	 */
	File getFile();

	/**
	 * @return a new {@code Sheet} instance
	 */
	Sheet createSheet();

	/**
	 * @return amount of {@code Sheet} instances in this workbook
	 */
	int countSheets();

	/**
	 * @param index
	 * @return {@code Sheet} instance at given index, or {@code null} if none exists
	 */
	Sheet getSheet(int index);

	/**
	 * @return the index of the sheet in this workbook
	 */
	int indexOf(Sheet sheet);

	/**
	 * The function that writes the sheet to a XML file.
	 * <div><b>Author:</b><br>
	 * <ul>
	 * <li>Jim Hommes</li>
	 * </ul>
	 * </div>
	 * @param file
	 * @throws XMLStreamException
	 *             If there was an error occurred writing XML
	 * @throws FactoryConfigurationError
	 *             if an instance of this factory cannot be loaded
	 * @throws IOException
	 *             If there was an error writing the file in the correct encoding
	 */
	void write(File file) throws XMLStreamException, FactoryConfigurationError, IOException;
}
