package GUI;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.print.PrinterException;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import File.Cell;
import File.Sheet.Range;

/**
 * 
 */
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
	
	private static int DEFAULT_TOOLBAR_HEIGHT = 25;
	
	private final ToolBarToggleButton Bold;
	private final ToolBarToggleButton Italic;
	private final ToolBarToggleButton Underlined;
	
	/**
	 * Construct a new SToobar
	 * @param parent
	 */
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
	
	/**
	 * Create the selection listener, such that the toggles in the
	 * toolbar are updated when the selection changes.
	 * @param table
	 */
	public void createSelectionListener(STable table) {
		table.addListSelectionListener(new ListSelectionListener() {

			@Override
			public void valueChanged(ListSelectionEvent arg0) {
				boolean bold = false, italic = false, underlined = false;
				Range selection = window.getSelectedRange();
				
				if ( selection != null ) {
					Cell first = selection.firstCell();
					if ( first != null ) {
						bold = first.isBold();
						italic = first.isItalic();
						underlined = first.isUnderlined();
					}
				}

				Bold.setSelected(bold);
				Italic.setSelected(italic);
				Underlined.setSelected(underlined);
			}
			
		});
		
	}

	private AbstractAction fileNew = new AbstractAction(null, icoNew) {
		@Override
		public void actionPerformed(ActionEvent e) {
			new Window();
		}
	};

	private AbstractAction fileOpen = new AbstractAction(null, icoOpen) {
		
		@Override
		public void actionPerformed(ActionEvent e) {
			window.FileOpen();
		}
		
	};

	private AbstractAction fileSave = new AbstractAction(null, icoSave) {
		
		@Override
		public void actionPerformed(ActionEvent e) {
			window.FileSave();
		}
		
	};

	private AbstractAction filePrint = new AbstractAction(null, icoPrint) {
		@Override
		public void actionPerformed(ActionEvent e) {
			try {
				window.getCurrentTable().print();
			} catch (PrinterException e1) {
				JOptionPane.showMessageDialog(null, e1.getMessage());
			}
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
	
	AbstractAction insertFunction = new AbstractAction(null, icoFunction) {
		@Override
		public void actionPerformed(ActionEvent e) {
			//TODO toon Insert Function dialog!
			new SFormulePicker(window);
		}
	};
	
	
	
	public class ToolBarButton extends JButton {
		
		public ToolBarButton(Action action) {
			super(action);
		}

		@Override
		public Dimension getMaximumSize() {
			return new Dimension(DEFAULT_TOOLBAR_HEIGHT, DEFAULT_TOOLBAR_HEIGHT);
		}

		@Override
		public Dimension getMinimumSize() {
			return new Dimension(DEFAULT_TOOLBAR_HEIGHT, DEFAULT_TOOLBAR_HEIGHT);
		}

		@Override
		public Dimension getPreferredSize() {
			return new Dimension(DEFAULT_TOOLBAR_HEIGHT, DEFAULT_TOOLBAR_HEIGHT);
		}
	}

	public class ToolBarToggleButton extends JToggleButton {

		ToolBarToggleButton(Action action) {
			super(action);
		}

		@Override
		public Dimension getMaximumSize() {
			return new Dimension(DEFAULT_TOOLBAR_HEIGHT, DEFAULT_TOOLBAR_HEIGHT);
		}

		@Override
		public Dimension getMinimumSize() {
			return new Dimension(DEFAULT_TOOLBAR_HEIGHT, DEFAULT_TOOLBAR_HEIGHT);
		}

		@Override
		public Dimension getPreferredSize() {
			return new Dimension(DEFAULT_TOOLBAR_HEIGHT, DEFAULT_TOOLBAR_HEIGHT);
		}
	}
}
