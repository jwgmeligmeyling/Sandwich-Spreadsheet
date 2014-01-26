package GUI;

import javax.swing.*;
import javax.swing.filechooser.FileFilter;
import javax.swing.filechooser.FileNameExtensionFilter;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.xml.sax.SAXException;

import java.awt.*;
import java.io.File;
import java.io.IOException;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;
import File.Workbook;
import Interfaces.ExceptionListener;

/**
 * The Window class
 */
@SuppressWarnings("serial")
public class Window extends JFrame implements ExceptionListener {
	
	private final SToolbar tbMain;
	private final FormuleBalk formule;
	private final JTabbedPane tabbedPane;
	private final SStatusBar statusBar;
	private final Workbook newFile;
	
	private static final int DEFAULT_WIDTH = 800;
	private static final int DEFAULT_HEIGHT = 500;
	private static final Color DEFAULT_BACKGROUND = new Color(240,240,240);
	
	/**
	 * Consturctor for the GUI
	 */
	public Window() {
		this(new Workbook(new Sheet()));
	}
	
	/**
	 * Constructor for the GUI.
	 * @param spreadsheet
	 * @throws HeadlessException
	 */
	public Window(Workbook spreadsheet) throws HeadlessException {
		super("Sandwich Spreadsheet - " + spreadsheet.getName());
		newFile = spreadsheet;
		
		setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		tbMain = new SToolbar(this);
		tabbedPane = new JTabbedPane();
		statusBar = new SStatusBar();
		formule = new FormuleBalk();
		SMenuBar smenubar = new SMenuBar(this);
		setJMenuBar(smenubar);
		tbMain.add(formule);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		container.add(tbMain, BorderLayout.PAGE_START);
		container.add(statusBar, BorderLayout.PAGE_END);
		container.add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.setTabPlacement(JTabbedPane.BOTTOM);
		
		paintSheets();
	}
	
	/**
	 * @return the {@code Sheet} instance in the active {@code JTabbledPane} tab
	 */
	public Sheet getCurrentSheet() {
		return newFile.getSheet(tabbedPane.getSelectedIndex());
	}
	
	/**
	 * @return the {@code Workbook} associated with this {@code Window}
	 */
	public Workbook getCurrentSpreadSheetFile(){
		return newFile;
	}
	
	/**
	 * @return the {@code STable} instance in the active {@code JTabbledPane} tab
	 */
	public STable getCurrentTable() {
		return getCurrentSheet().getSTable();
	}
	
	/**
	 * Update the active table
	 */
	public void updateTable() {
		getCurrentTable().updateUI();
	}
	
	/**
	 * @return a new {@code Sheet} in this {@code Workbook}
	 */
	public Sheet createSheet() {
		return newFile.createSheet();
	}
	
	/**
	 * @return The selected {@code  Range} or null if no selection
	 */
	public Range getSelectedRange() {
		return getCurrentTable().getSelectedRange();
	}
	
	/**
	 * @return get the selected cell, returns null if no selected cell or
	 *         multiple selected cell
	 */
	public Cell getSelectedCell() {
		Range range = getSelectedRange();
		if ( range != null &&  range.isSingleCell() ) {
			return range.firstCell();
		} else {
			return null;
		}
	}
	
	/**
	 * Go to a {@code Sheet}
	 * @param sheet
	 */
	public void goToSheet(Sheet sheet) {
		tabbedPane.setSelectedIndex(newFile.indexOf(sheet));
	}
	
	/**
	 * Construct (paint) all tabs for the sheets in the current Workbook
	 */
	private void paintSheets() {
		for(Sheet sheet : newFile.getSheets()){
			paintSheet(sheet);
		}
	}
	
	public FormuleBalk getFormuleBalk() {
		return formule;
	}
	
	/**
	 * Construct (paint) a tab in the tabbed pane for a new Sheet
	 * @param sheet
	 */
	public void paintSheet(Sheet sheet) {
		STable table = new STable(sheet, this);
		
		Box box = Box.createVerticalBox();
		box.add(table.getTableHeader());
		JScrollPane scrollPane = new JScrollPane(table);
		scrollPane.setBackground(DEFAULT_BACKGROUND);
		new FixedColumnTable(1, scrollPane);
		box.add(scrollPane);
		
		tabbedPane.addTab(sheet.getSheetName(), box);
		tbMain.createSelectionListener(table);
	}

	/**
	 * @return the {@code JTabbedPane} for this {@code Window} such that
	 * the actions can access it.
	 */
	public JTabbedPane getTabbedPane() {
		return tabbedPane;
	}

	/**
	 * Set tab placement
	 * @param setting
	 */
	public void setTabPlacement(int setting) {
		tabbedPane.setTabPlacement(setting);
	}
	
	/**
	 * @return the {@code StatusBar} for this {@code Window}
	 */
	public JLabel getStatusBar() {
		return statusBar;
	}
	
	/**
	 * Set the visibility for the statusbar
	 * @param visible
	 */
	public void setStatusBarVisibility(boolean visible) {
		statusBar.setVisible(visible);
	}
	
	/**
	 * Set the contents for the statusbar
	 * @param text
	 */
	public void setStatusBar(String text) {
		statusBar.setText(text);
	}
	
	@Override
	public void onException(Exception e) {
		setStatusBar(e.getMessage() == null ? "Unknown exception occured: " + e.toString() : e.getMessage());
	}

	/**
	 * Open a file in a new Window.
	 */
	public void FileOpen() {
		// Open een dialog
		JFileChooser fc = new JFileChooser();
		
		FileFilter filterAll = new FileNameExtensionFilter("All acceptable file formats", "xSwSht", "xml");
		FileFilter filterSW = new FileNameExtensionFilter("Sandwich Spreadsheet Document", "xSwSht");
		FileFilter filterXML = new FileNameExtensionFilter("Extensible Markup Language", "xml");
		
		fc.addChoosableFileFilter(filterAll);
		fc.addChoosableFileFilter(filterSW);
		fc.addChoosableFileFilter(filterXML);
		fc.setFileFilter(filterSW);
		
		int returnVal = fc.showOpenDialog(this);
		
		// Wanneer niet op cancel gedrukt:
	    if(returnVal == JFileChooser.APPROVE_OPTION) {
			File file = fc.getSelectedFile();
			
			try {
				// Nieuwe workbook aanmaken vanuit de XML.
				new Window(new Workbook(file));
				
			} catch (ParserConfigurationException e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage());
				e1.printStackTrace();
			} catch (SAXException e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage());
				e1.printStackTrace();
			} catch (IOException e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage());
				e1.printStackTrace();
			} 
	    }	    
	}

	/**
	 * Save the file in this Window.
	 */
	public void FileSave() {
		JFileChooser fc = new JFileChooser();

		FileFilter filterAll = new FileNameExtensionFilter("All acceptable file formats", "xSwSht", "xml");
		FileFilter filterSW = new FileNameExtensionFilter("Sandwich Spreadsheet Document", "xSwSht");
		FileFilter filterXML = new FileNameExtensionFilter("Extensible Markup Language", "xml");
		
		fc.addChoosableFileFilter(filterAll);
		fc.addChoosableFileFilter(filterSW);
		fc.addChoosableFileFilter(filterXML);
		fc.setFileFilter(filterSW);
		
		Workbook workbook = this.getCurrentSpreadSheetFile();
		File file = workbook.getFile();

		if (file == null) {
			int returnVal = fc.showSaveDialog(this);
			if (returnVal == JFileChooser.APPROVE_OPTION) {
				file = fc.getSelectedFile();
				String path = file.getPath();

				if (!path.toLowerCase().endsWith(".xSwSht")) {
					file = new File(path + ".xSwSht");
				}

			}
		}

		if (file == null) {
			return;
		}

		try {
			workbook.write(file);
		} catch (XMLStreamException e1) {
			JOptionPane.showMessageDialog(null, e1.getMessage());
			e1.printStackTrace();
		} catch (FactoryConfigurationError e1) {
			JOptionPane.showMessageDialog(null, e1.getMessage());
			e1.printStackTrace();
		} catch (IOException e1) {
			JOptionPane.showMessageDialog(null, e1.getMessage());
			e1.printStackTrace();
		}
	}

	/**
	 * Open a new Window with a new empty Spreadsheet.
	 * @param args
	 */
	public static void main(String[] args) {
		new Window();
	}
}