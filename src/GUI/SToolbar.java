package GUI;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.io.IOException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.stream.FactoryConfigurationError;
import javax.xml.stream.XMLStreamException;

import org.xml.sax.SAXException;

import File.Cell;
import File.SpreadSheetFile;
import File.XMLRead;
import Interfaces.Range;

@SuppressWarnings("serial")
public class SToolbar extends JToolBar {
	
	private static ImageIcon icoNew = new ImageIcon("img/new.png", "New");
	private static ImageIcon icoOpen = new ImageIcon("img/open.png", "Open");
	private static ImageIcon icoSave = new ImageIcon("img/save.png", "Save");
	private static ImageIcon icoBold = new ImageIcon("img/text-bold.png", "Bold");
	private static ImageIcon icoItalic = new ImageIcon("img/text-italic.png", "Italic");
	private static ImageIcon icoUnderlined = new ImageIcon("img/text-underlined.png", "Underlined");
	private static ImageIcon icoPrint = new ImageIcon("img/print.png", "Print");
	private static ImageIcon icoBColor = new ImageIcon("img/backcolor.gif", "Background color");
	private static ImageIcon icoFColor = new ImageIcon("img/forecolor.gif", "Text color");
	private static ImageIcon icoFunction = new ImageIcon("img/function.gif", "Insert function dialog");
	
	private final Window window;
	
	private int toolbarHeight = 25;
	
	private final ToolBarToggleButton Bold;
	private final ToolBarToggleButton Italic;
	private final ToolBarToggleButton Underlined;
	
	private final JFileChooser fc = new JFileChooser();
	//private final FileNameExtensionFilter filter = new FileNameExtensionFilter("xml");
	
	
	public SToolbar(Window parent) {
		super();
		this.window = parent;
		setFloatable(false);
		
		add(new ToolBarButton(fileNew));
		add(new ToolBarButton(fileOpen));
		add(new ToolBarButton(fileSave));
		add(new ToolBarButton(filePrint));
		addSeparator();
		add(new ToolBarButton(formatFColor));
		add(new ToolBarButton(formatBColor));
		addSeparator();
		Bold=new ToolBarToggleButton(formatBold);
		add(Bold);
		Italic=new ToolBarToggleButton(formatItalic);
		add(Italic);
		Underlined=new ToolBarToggleButton(formatUnderline);
		add(Underlined);
		addSeparator();
		add(new ToolBarButton(insertFunction));
		
	}
	
	public void createSelectionListener(STable table) {
		if ( table == null )
			return;
		ListSelectionModel lsm = table.getSelectionModel();
		if ( lsm == null )
			return;
		lsm.addListSelectionListener(new ListSelectionListener() {

			@Override
			public void valueChanged(ListSelectionEvent arg0) {
				Bold.setSelected(window.getSelectedRange().firstCell().isBold());
				Italic.setSelected(window.getSelectedRange().firstCell().isItalic());
				Underlined.setSelected(window.getSelectedRange().firstCell().isUnderlined());
			}
			
		});
		
	}

	public int getToolbarHeight() {
		return toolbarHeight;
	}

	public void setToolbarHeight(int toolbarHeight) {
		this.toolbarHeight = toolbarHeight;
	}

	private AbstractAction fileNew = new AbstractAction(null, icoNew) {
		@Override
		public void actionPerformed(ActionEvent e) {
			new Window("Sandwich Spreadsheet", new SpreadSheetFile());
		}
	};

	private AbstractAction fileOpen = new AbstractAction(null, icoOpen) {
		
		@Override
		public void actionPerformed(ActionEvent e) {
			// Open een dialog
			//fc.setFileFilter(filter);
			int returnVal = fc.showOpenDialog(window);
			
			// Wanneer niet op cancel gedrukt:
		    if(returnVal == JFileChooser.APPROVE_OPTION) {
				String path = fc.getSelectedFile().getPath();
				
				try {
					// Nieuwe sheetfile aanmaken vanuit de XML
					SpreadSheetFile sheetfile = XMLRead.read(path);
					new Window("Sandwich Spreadsheet", sheetfile);
					
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
	};

	private AbstractAction fileSave = new AbstractAction(null, icoSave) {
		@Override
		public void actionPerformed(ActionEvent e) {
			int returnVal = fc.showSaveDialog(window);
		    if(returnVal == JFileChooser.APPROVE_OPTION) {
				String path = fc.getSelectedFile().getPath();

				System.out.println("You chose to save to this location: "
						+ path);

				SpreadSheetFile sheetfile = window.getCurrentSpreadSheetFile();

				try {

					sheetfile.write(path);

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
		}
	};

	private AbstractAction filePrint = new AbstractAction(null, icoPrint) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Print";
			JOptionPane.showMessageDialog(null, st);
		}
	};

	private AbstractAction formatBold = new AbstractAction(null, icoBold) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Range range = window.getSelectedRange();
			Cell[] selectedCells = range == null ? new Cell[0] : range.getCellArray();
			
			boolean value = ! selectedCells[0].isBold();
			
			for ( Cell cell : selectedCells ) {
				cell.setBold(value);
			}
			
			window.updateTable();
			
//			String st = "Bold";
//			JOptionPane.showMessageDialog(null, st);
		}
	};

	private AbstractAction formatItalic = new AbstractAction(null, icoItalic) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Range range = window.getSelectedRange();
			Cell[] selectedCells = range == null ? new Cell[0] : range.getCellArray();
			
			boolean value = ! selectedCells[0].isItalic();
			
			for ( Cell cell : selectedCells ) {
				cell.setItalic(value);
			}
			
			window.updateTable();
			
			//String st = "Italic";
			//JOptionPane.showMessageDialog(null, st);
		}
	};

	private AbstractAction formatUnderline = new AbstractAction(null, icoUnderlined) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Range range = window.getSelectedRange();
			Cell[] selectedCells = range == null ? new Cell[0] : range.getCellArray();
			
			boolean value = ! selectedCells[0].isUnderlined();
			
			for ( Cell cell : selectedCells ) {
				cell.setUnderlined(value);
			}
			
			window.updateTable();
		}
	};
	
	private AbstractAction formatBColor = new AbstractAction(null, icoBColor) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Color oldColor = new Color(50, 200, 160);
			
			new SColorPicker("Choose a background color", oldColor,window,true);
			//TODO bColor van selected cells instellen
		}
	};
	
	private AbstractAction formatFColor = new AbstractAction(null, icoFColor) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Color oldColor = new Color(50, 200, 160);
			new SColorPicker("Choose a text color", oldColor,window,false);
			//TODO fColor van selected cells instellen
		}
	};
	
	private AbstractAction insertFunction = new AbstractAction(null, icoFunction) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Insert Function";
			 new SFormulePicker( window ).show();;
			//TODO toon Insert Function dialog!
		}
	};
	
	
	
	public class ToolBarButton extends JButton {
		public ToolBarButton(Action action) {
			super(action);
		}

		/*
		@Deprecated
		public ToolBarButton(String string) {
			super(string);
		}
		*/

		@Override
		public Dimension getMaximumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}

		@Override
		public Dimension getMinimumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}

		@Override
		public Dimension getPreferredSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}
	}

	public class ToolBarToggleButton extends JToggleButton {

		ToolBarToggleButton(Action action) {
			super(action);
		}

		@Override
		public Dimension getMaximumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}

		@Override
		public Dimension getMinimumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}

		@Override
		public Dimension getPreferredSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}
	}
}