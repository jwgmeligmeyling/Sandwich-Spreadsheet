package GUI;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ButtonGroup;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JRadioButton;
import javax.swing.JTabbedPane;

@SuppressWarnings("serial")
public class SMenuBar extends JMenuBar {
	
	private static ImageIcon icoNew = new ImageIcon("img/new.png", "New");
	private static ImageIcon icoOpen = new ImageIcon("img/open.png", "Open");
	private static ImageIcon icoSave = new ImageIcon("img/save.png", "Save");
	private static ImageIcon icoPrint = new ImageIcon("img/print.png", "Print");
	
	private JTabbedPane tabbedPane;
	private final Window window;

	public SMenuBar(Window window) {
		super();
		this.window = window;
		setTabbedPane(window.getTabbedPane());
		createFileMenu();
		createEditMenu();
		createViewMenu();
		createInsertMenu();
		createHelpMenu();
	}
	
	private void createFileMenu() {
		JMenu menu = new JMenu("File");
		menu.setMnemonic(Mnemonic.FILE.value);
		
		menu.add(new MenuItem("New", icoNew, Mnemonic.NEW));
		menu.add(new MenuItem(Mnemonic.OPEN, fileOpen));
		menu.add(new MenuItem(Mnemonic.SAVE, fileSave));
		menu.add(new MenuItem(Mnemonic.SAVE_AS, fileSaveAs));
		
		menu.addSeparator();
		menu.add(new MenuItem("Print", icoPrint, Mnemonic.PRINT));
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
		menu.add(new MenuItem("Zoom in/out", Mnemonic.ZOOM));
		menu.add(new MenuItem("Show/hide statusbar", Mnemonic.STATUS_BAR));
		
		ButtonGroup jbgTabsTopBottom = new ButtonGroup();
		JRadioButton jrbTabsTop = new JRadioButton("Tabs on top");
		JRadioButton jrbTabsBottom = new JRadioButton("Tabs on bottom", true);
		
		jrbTabsTop.setAction(ViewTabsTop_Click);
		jrbTabsBottom.setAction(ViewTabsBottom_Click);
		
		jbgTabsTopBottom.add(jrbTabsTop);
		jbgTabsTopBottom.add(jrbTabsBottom);

		menu.add(jrbTabsTop);
		menu.add(jrbTabsBottom);
		
		this.add(menu);
	}
	
	private void createInsertMenu() {
		JMenu menu = new JMenu("Insert");
		menu.setMnemonic(Mnemonic.INSERT.value);
		menu.add(new MenuItem("Insert Function", Mnemonic.INSERT_FUNCTION));
		menu.add(new MenuItem("Create new Graph", Mnemonic.CREATE_GRAPH));
		menu.add(new MenuItem(Mnemonic.CREATE_SHEET, InsertWorksheet_Click));
		this.add(menu);
	}
	
	private void createHelpMenu() {
		JMenu menu = new JMenu("Help");
		menu.setMnemonic(Mnemonic.HELP.value);
		menu.add(new MenuItem("Help", Mnemonic.GET_HELP));
		menu.add(new MenuItem("About", Mnemonic.ABOUT));
		this.add(menu);
	}
	
	public JTabbedPane getTabbedPane() {
		return tabbedPane;
	}

	public void setTabbedPane(JTabbedPane tabbedPane) {
		this.tabbedPane = tabbedPane;
	}

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
	
	private AbstractAction exit = new AbstractAction("Exit") {

		@Override
		public void actionPerformed(ActionEvent e) {
			int answer = JOptionPane.showConfirmDialog(null,
					"Are you sure you want to quit?", "Please confirm",
					JOptionPane.YES_NO_OPTION);
			if (answer == JOptionPane.YES_OPTION) {
				System.exit(0);
			}		
		}
		
	};

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
	
	private AbstractAction ViewTabsTop_Click = new AbstractAction("Tabs on top") {

		@Override
		public void actionPerformed(ActionEvent e) {
			tabbedPane.setTabPlacement(JTabbedPane.TOP);
		}
		
	};
	
	private AbstractAction InsertWorksheet_Click = new AbstractAction("Create new Sheet") {

		@Override
		public void actionPerformed(ActionEvent e) {
			window.createSheet();	
		}
		
	};
	
	private AbstractAction ViewTabsBottom_Click = new AbstractAction("Tabs on bottom") {

		@Override
		public void actionPerformed(ActionEvent e) {
			tabbedPane.setTabPlacement(JTabbedPane.BOTTOM);		
		}
		
	};
	
	public static enum Mnemonic {
		FILE('f'), NEW('n'), OPEN('o'), SAVE('s'), SAVE_AS('a'), PRINT('p'), EXIT('e'),
	
		EDIT('e'), UNDO('z'), REDO('y'), CUT('x'), COPY('c'), PASTE('v'),
		
		VIEW('v'), ZOOM('z'), STATUS_BAR('s'),
		
		INSERT('i'), INSERT_FUNCTION('F'), CREATE_GRAPH('g'), CREATE_SHEET('s'),
		
		HELP('h'), GET_HELP('h'), ABOUT('a');
		
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