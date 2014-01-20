package GUI;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JCheckBox;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;

import File.Sheet;

@SuppressWarnings("serial")
public class SMenuBar extends JMenuBar {
	
	private static ImageIcon icoNew = new ImageIcon("img/new.png", "New");
	private static ImageIcon icoOpen = new ImageIcon("img/open.png", "Open");
	private static ImageIcon icoSave = new ImageIcon("img/save.png", "Save");
	private static ImageIcon icoPrint = new ImageIcon("img/print.png", "Print");
	
	private final Window window;

	public SMenuBar(Window window) {
		super();
		this.window = window;
		createFileMenu();
		createEditMenu();
		createViewMenu();
		createInsertMenu();
		createHelpMenu();
	}
	
	private void createFileMenu() {
		JMenu menu = new JMenu("File");
		menu.setMnemonic(Mnemonic.FILE.value);
		
		menu.add(new MenuItem(Mnemonic.NEW, fileNew));
		menu.add(new MenuItem(Mnemonic.OPEN, fileOpen));
		menu.add(new MenuItem(Mnemonic.SAVE, fileSave));
		menu.add(new MenuItem(Mnemonic.SAVE_AS, fileSaveAs));
		menu.addSeparator();
		menu.add(new MenuItem(Mnemonic.PRINT, filePrint));
		menu.addSeparator();
		menu.add(new MenuItem(Mnemonic.EXIT, exit));
		
		this.add(menu);
	}
	
	private void createEditMenu() {
		JMenu menu = new JMenu("Edit");
		menu.setMnemonic(Mnemonic.EDIT.value);
		menu.add(new MenuItem(Mnemonic.UNDO, undo));
		menu.add(new MenuItem(Mnemonic.REDO, redo));
		menu.add(new MenuItem(Mnemonic.CUT, cut));
		menu.add(new MenuItem(Mnemonic.COPY, copy));
		menu.add(new MenuItem(Mnemonic.PASTE, paste));
		this.add(menu);
	}
	
	private void createViewMenu() {
		JMenu menu = new JMenu("View");
		menu.setMnemonic(Mnemonic.VIEW.value);
		
		JCheckBox jcbShowStatusBar = new JCheckBox("Show Status Bar");
		jcbShowStatusBar.setSelected(window.getStatusBar().isVisible());
		jcbShowStatusBar.setAction(ViewShowStatusBar_Click);
		
		menu.add(jcbShowStatusBar);
		
		ButtonGroup jbgTabsTopBottom = new ButtonGroup();
		JRadioButton jrbTabsTop = new JRadioButton("Tabs on top");
		jbgTabsTopBottom.add(jrbTabsTop);
		JRadioButton jrbTabsBottom = new JRadioButton("Tabs on bottom", true);
		jbgTabsTopBottom.add(jrbTabsBottom);
		
		jrbTabsTop.setAction(ViewTabsTop_Click);
		jrbTabsBottom.setAction(ViewTabsBottom_Click);
		
		menu.add(jrbTabsTop);
		menu.add(jrbTabsBottom);
		
		this.add(menu);
	}
	
	private void createInsertMenu() {
		JMenu menu = new JMenu("Insert");
		menu.setMnemonic(Mnemonic.INSERT.value);
		menu.add(new MenuItem(Mnemonic.INSERT_FUNCTION, InsertFunction_Click));
		menu.add(new MenuItem("Create new Graph", Mnemonic.CREATE_GRAPH));
		menu.add(new MenuItem(Mnemonic.CREATE_SHEET, InsertWorksheet_Click));
		this.add(menu);
	}
	
	private void createHelpMenu() {
		JMenu menu = new JMenu("About");
		menu.setMnemonic(Mnemonic.ABOUT.value);
		//menu.add(new MenuItem("Help", Mnemonic.GET_HELP));
		menu.add(new MenuItem("Info", Mnemonic.INFO));
		this.add(menu);
	}
	
	private AbstractAction fileNew = new AbstractAction("New file", icoNew) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>New";
			JOptionPane.showMessageDialog(null, st);
		}
	};
	
	
	private AbstractAction fileOpen = new AbstractAction("Open", icoOpen) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Open";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction fileSave = new AbstractAction("Save", icoSave) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Save";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction fileSaveAs = new AbstractAction("Save as", icoSave) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>SaveAs";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction filePrint = new AbstractAction("Print", icoPrint) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Print";
			JOptionPane.showMessageDialog(null, st);
		}
	};
	
	
	private AbstractAction exit = new AbstractAction("Exit") {
		@Override
		public void actionPerformed(ActionEvent e) {
			int answer = JOptionPane.showConfirmDialog(null, "Are you sure you want to quit?", "Please confirm", JOptionPane.YES_NO_OPTION);
			if (answer == JOptionPane.YES_OPTION) {
				System.exit(0);
			}
		}
	};
	
	// Edit menu:
	
	private AbstractAction undo = new AbstractAction("Undo") {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>undo";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction redo = new AbstractAction("Redo") {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>redo";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction cut = new AbstractAction("Cut") {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>cut";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction copy = new AbstractAction("Copy") {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>copy";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	private AbstractAction paste = new AbstractAction("Paste") {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>paste";
			JOptionPane.showMessageDialog(null, st);			
		}
	};
	
	// View menu:
	
	private AbstractAction ViewShowStatusBar_Click = new AbstractAction("Show statusbar") {
		@Override
		public void actionPerformed(ActionEvent e) {
			window.getStatusBar().setVisible(((JCheckBox) e.getSource()).isSelected());
		}
	};
	
	private AbstractAction ViewTabsTop_Click = new AbstractAction("Tabs on top") {
		@Override
		public void actionPerformed(ActionEvent e) {
			window.getTabbedPane().setTabPlacement(JTabbedPane.TOP);
		}
	};

	private AbstractAction ViewTabsBottom_Click = new AbstractAction("Tabs on bottom") {
		@Override
		public void actionPerformed(ActionEvent e) {
			window.getTabbedPane().setTabPlacement(JTabbedPane.BOTTOM);
		}
	};

	/*
	 * Insert menu:
	 */
	
	private AbstractAction InsertWorksheet_Click = new AbstractAction("Create new Sheet") {
		@Override
		public void actionPerformed(ActionEvent e) {
			Sheet sheet = window.createSheet();
			window.paintSheet(sheet);
			window.goToSheet(sheet);
		}
	};
	
	private AbstractAction InsertFunction_Click = new AbstractAction("Insert Function") {
		@Override
		public void actionPerformed(ActionEvent e) {
			//TODO toon Insert Function dialog!
			new SFormulePicker(window);
		}
	};
	
	public static enum Mnemonic {
		FILE('f'), NEW('n'), OPEN('o'), SAVE('s'), SAVE_AS('a'), PRINT('p'), EXIT('e'),
		EDIT('e'), UNDO('z'), REDO('y'), CUT('x'), COPY('c'), PASTE('v'),
		VIEW('v'), ZOOM('z'), STATUS_BAR('s'),
		INSERT('i'), INSERT_FUNCTION('F'), CREATE_GRAPH('g'), CREATE_SHEET('s'),
		ABOUT('a'), GET_HELP('h'), INFO('i');
		
		private final char value;
		
		Mnemonic(char value) {
			this.value = value;
		}
	}

	public static class MenuItem extends JMenuItem {
		@Deprecated
		private MenuItem(String text) {
			this(text, (Icon) null);
		}
		
		@Deprecated
		private MenuItem(String text, Icon icon) {
			super(text, icon);
		}
		
		@Deprecated
		private MenuItem(String text, Mnemonic mnemonic) {
			this(text);
			setMnemonic(mnemonic.value);
		}
		
		@Deprecated
		private MenuItem(String text, Icon icon, Mnemonic mnemonic) {
			super(text, icon);
			setMnemonic(mnemonic.value);
		}
		
		private MenuItem(Action action) {
			super(action);
		}
		
		private MenuItem(Mnemonic mnemonic, Action action) {
			super(action);
			setMnemonic(mnemonic.value);
		}
	}
}
