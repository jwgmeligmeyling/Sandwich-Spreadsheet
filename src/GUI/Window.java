package GUI;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import File.Sheet;
import File.SpreadSheetFile;

@SuppressWarnings("serial")
public class Window extends JFrame implements ActionListener {
	
	// ImageIcon declaration
	private static ImageIcon icoNew = 			new ImageIcon("img/new.png", "New");
	private static ImageIcon icoOpen = 			new ImageIcon("img/open.png", "Open");
	private static ImageIcon icoSave = 			new ImageIcon("img/save.png", "Save");
	private static ImageIcon icoBold = 			new ImageIcon("img/text-bold.png", "Bold");               
	private static ImageIcon icoItalic = 		new ImageIcon("img/text-italic.png", "Italic");           
	private static ImageIcon icoUnderlined = 	new ImageIcon("img/text-underlined.png", "Underlined");
	private static ImageIcon icoPrint = 		new ImageIcon("img/print.png", "Print");
	
	private static JMenuBar jmb = new JMenuBar();
	
	private static JMenu menuFile = new JMenu("File");
		private static JMenuItem jmiOpen = new JMenuItem("Open", icoOpen);
		private static JMenuItem jmiSave = new JMenuItem("Save", icoSave);
		private static JMenuItem jmiSaveAs = new JMenuItem("Save As", icoSave);
		private static JMenuItem jmiExit = new JMenuItem("Exit");
		
	private static JMenu menuEdit = new JMenu("Edit");
		private static JMenuItem jmiUndo = new JMenuItem("Undo");
		private static JMenuItem jmiRedo = new JMenuItem("Redo");
		private static JMenuItem jmiCut = new JMenuItem("Cut");
		private static JMenuItem jmiCopy = new JMenuItem("Copy");
		private static JMenuItem jmiPaste = new JMenuItem("Paste");
		
	private static JMenu menuView = new JMenu("View");
		private static JMenuItem jmiZoom = new JMenuItem("Zoom in/out");
		private static JCheckBox jmiStatusBar = new JCheckBox("Show/hide Status Bar");
		//private static JMenu jmTabsTopBottom = new JMenu("Show tabs on top/bottom");
		private static ButtonGroup jbgTabsTopBottom = new ButtonGroup();
		private static JRadioButton jrbTabsTop = new JRadioButton("Tabs on top");
		private static JRadioButton jrbTabsBottom = new JRadioButton("Tabs on bottom", true);
		
	private static JMenu menuInsert = new JMenu("Insert");
		private static JMenuItem jmiInsFunction = new JMenuItem("Insert Function");
		private static JMenuItem jmiInsGraph = new JMenuItem("Create new Graph");
		private static JMenuItem jmiInsSheet = new JMenuItem("Create new Sheet");
		
	private static JMenu menuHelp = new JMenu("Help");
		private static JMenuItem jmiHelp = new JMenuItem("Help");
		private static JMenuItem jmiAbout = new JMenuItem("About");
		
	// Toolbar declaration
	private static JToolBar tbMain = new JToolBar();
		private static JButton tbnNew = new JButton(icoNew);
		private static JButton tbnOpen = new JButton(icoOpen);
		private static JButton tbnSave = new JButton(icoSave);
		private static JButton tbnPrint = new JButton(icoPrint);
		private static JButton tbnFColor = new JButton("F color");	// ColorPicker!!
		private static JButton tbnBColor = new JButton("B color");	// ColorPicker!!
		private static JToggleButton ttgBold = new JToggleButton(icoBold);
		private static JToggleButton ttgItalic = new JToggleButton(icoItalic);
		private static JToggleButton ttgUnderlined = new JToggleButton(icoUnderlined);
		private static JTextField formule = new JTextField();
	
	private static JTabbedPane tabs = new JTabbedPane();
	
	private static SpreadSheetFile newFile;
	
	/**
	 * Constructor for the GUI.
	 * @param title is the title of the window.
	 * @throws HeadlessException
	 */
	public Window(String title) throws HeadlessException {
		
		super(title);
		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		setJMenuBar(jmb);
		
		jmb.add(menuFile);			menuFile.setMnemonic('f');
			menuFile.add(jmiOpen);		jmiOpen.setMnemonic('o');
			menuFile.add(jmiSave);		jmiSave.setMnemonic('s');
			menuFile.add(jmiSaveAs);	jmiSaveAs.setMnemonic('a');
			menuFile.add(jmiExit);		jmiExit.setMnemonic('s');
			
		jmb.add(menuEdit);			menuEdit.setMnemonic('e');
			menuEdit.add(jmiUndo);		jmiUndo.setMnemonic('u');
			menuEdit.add(jmiRedo);		jmiRedo.setMnemonic('r');
			menuEdit.add(jmiCut);		jmiCut.setMnemonic('x');
			menuEdit.add(jmiCopy);		jmiCopy.setMnemonic('c');
			menuEdit.add(jmiPaste);		jmiPaste.setMnemonic('v');
			
		jmb.add(menuView);			menuView.setMnemonic('v');
			menuView.add(jmiZoom);		jmiZoom.setMnemonic('z');
			menuView.add(jmiStatusBar);	jmiStatusBar.setMnemonic('s');
			menuView.addSeparator();
			//menuView.add(jmTabsTopBottom);	jmTabsTopBottom.setMnemonic('t');
			menuView.add(jrbTabsTop);		jrbTabsTop.setMnemonic('t');		jbgTabsTopBottom.add(jrbTabsTop);
			menuView.add(jrbTabsBottom);	jrbTabsBottom.setMnemonic('b');		jbgTabsTopBottom.add(jrbTabsBottom);
			
		jmb.add(menuInsert);			menuInsert.setMnemonic('i');
			menuInsert.add(jmiInsGraph);	jmiInsGraph.setMnemonic('g');
			menuInsert.add(jmiInsFunction);	jmiInsFunction.setMnemonic('f');
			menuInsert.add(jmiInsSheet);	jmiInsSheet.setMnemonic('s');
			
		jmb.add(menuHelp);			menuHelp.setMnemonic('h');
			menuHelp.add(jmiHelp);		jmiHelp.setMnemonic('h');
			menuHelp.add(jmiAbout);		jmiAbout.setMnemonic('a');
		
		// Adding the main toolbar
		container.add(tbMain, BorderLayout.PAGE_START);
			tbMain.setFloatable(false);
			tbMain.add(tbnNew);
			tbMain.add(tbnOpen);
			tbMain.add(tbnSave);
			tbMain.add(tbnPrint);
			tbMain.addSeparator();
			tbMain.add(tbnFColor);
			tbMain.add(tbnBColor);
			tbMain.addSeparator();
			tbMain.add(ttgBold);
			tbMain.add(ttgItalic);
			tbMain.add(ttgUnderlined);
			tbMain.add(formule);
		
		setLocationRelativeTo(null);
		pack();
		setVisible(true);
		
		add(tabs, BorderLayout.CENTER);
		tabs.setTabPlacement(JTabbedPane.BOTTOM);
		
		newFile = new SpreadSheetFile();
		
		createSheet();
		
		// Add ActionListeners to Events
		jmiOpen.addActionListener(this);
		jmiSave.addActionListener(this);
		jmiSaveAs.addActionListener(this);
		jmiExit.addActionListener(this);
		jmiUndo.addActionListener(this);
		jmiRedo.addActionListener(this);
		jmiCut.addActionListener(this);
		jmiCopy.addActionListener(this);
		jmiPaste.addActionListener(this);
		jmiZoom.addActionListener(this);
		jmiStatusBar.addActionListener(this);
		jrbTabsTop.addActionListener(this);
		jrbTabsBottom.addActionListener(this);
		jmiInsGraph.addActionListener(this);
		jmiInsFunction.addActionListener(this);
		jmiInsSheet.addActionListener(this);
		jmiHelp.addActionListener(this);
		jmiAbout.addActionListener(this);
		
		// Add ActionListeners to ToolBar
		tbnNew.addActionListener(this);
		tbnOpen.addActionListener(this);
		tbnSave.addActionListener(this);
		tbnPrint.addActionListener(this);
		tbnFColor.addActionListener(this);
		tbnBColor.addActionListener(this);
		ttgBold.addActionListener(this);
		ttgItalic.addActionListener(this);
		ttgUnderlined.addActionListener(this);
	}
	
	public static void createSheet() {
		System.out.println("countSheets() = " + newFile.countSheets());
		Sheet newSheet = newFile.newSheet("Sheet" + (newFile.countSheets() + 1));
		
		fillSheet(newSheet);
		
		JTable table = new STable(newSheet, formule);
		
		Box box = Box.createVerticalBox();
		box.add(table.getTableHeader());
		JScrollPane scrollPane = new JScrollPane(table);
		scrollPane.setPreferredSize(new Dimension(700,500));
		box.add(scrollPane);
		
		tabs.addTab(newSheet.getSheetName(), box);
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {	
		
		//MenuBar
			// Menu File
			if (e.getSource() == jmiOpen) {			Events.FileOpen_Click(); }
			if (e.getSource() == jmiSave) {			Events.FileSave_Click(); }
			if (e.getSource() == jmiSaveAs) {		Events.FileSaveAs_Click(); }
			if (e.getSource() == jmiExit) {			Events.FileExit_Click(); }
			// Menu Edit
			if (e.getSource() == jmiUndo) {			Events.EditUndo_Click(); }
			if (e.getSource() == jmiRedo) {			Events.EditRedo_Click(); }
			if (e.getSource() == jmiCut) {			Events.EditCut_Click(); }
			if (e.getSource() == jmiCopy) {			Events.EditCopy_Click(); }
			if (e.getSource() == jmiPaste) {		Events.EditPaste_Click(); }
			// Menu View
			if (e.getSource() == jmiZoom) {			Events.ViewZoom_Click(); }
			if (e.getSource() == jmiStatusBar) {	Events.ViewStatusBar_Click(); }
			if (e.getSource() == jrbTabsTop) {		Events.ViewTabsTop_Click(tabs); }
			if (e.getSource() == jrbTabsBottom) {	Events.ViewTabsBottom_Click(tabs); }
			// Menu Insert
			if (e.getSource() == jmiInsGraph) {		Events.InsertGraph_Click(); }
			if (e.getSource() == jmiInsFunction) {	Events.InsertFunction_Click(); }
			if (e.getSource() == jmiInsSheet) {		Events.InsertWorksheet_Click(); }
			// MenuHelp
			if (e.getSource() == jmiHelp) {			Events.HelpHelp_Click(); }
			if (e.getSource() == jmiAbout) {		Events.HelpAbout_Click(); }
		
		//ToolBar	
			if (e.getSource() == tbnNew           ) { Events.tbMainNew_Click(); }
			if (e.getSource() == tbnOpen          ) { Events.tbMainOpen_Click(); }
			if (e.getSource() == tbnSave          ) { Events.tbMainSave_Click(); }
			if (e.getSource() == tbnPrint         ) { Events.tbMainPrint_Click(); }
			if (e.getSource() == tbnFColor        ) { Events.tbMainFColor_Click(); }
			if (e.getSource() == tbnBColor        ) { Events.tbMainBColor_Click(); }
			if (e.getSource() == ttgBold          ) { Events.tbMainBold_Click(); }
			if (e.getSource() == ttgItalic        ) { Events.tbMainItalic_Click(); }
			if (e.getSource() == ttgUnderlined    ) { Events.tbMainUnderlined_Click(); }
	}
	
	public void setTabPlacement(int setting) {
		tabs.setTabPlacement(setting);
	}
	
	
	
	public static void main(String[] args) {
		new Window("Sandwich Spreadsheet");
	}
	
	public static void fillSheet(Sheet sheet) {
		sheet.createCell("bliep", 0, 0);
		sheet.createCell("5", 0, 1);
		sheet.createCell("=5", 0, 2);
		sheet.createCell("=5*2", 1, 0);
		sheet.createCell("=2+2*3", 1, 1);
		sheet.createCell("=SUM(5,3)", 1, 2);
		sheet.createCell("2+2", 2, 0);
		sheet.createCell("=PRODUCT(3,5,2)",1,1);
		sheet.createCell("", 2, 2);
		
		for ( int i = 10; i < 20; i++ ) {
			for ( int j = 10; j < 200; j++ ) {
				sheet.createCell("=RANDBETWEEN(0,100)", i, j);
			}
		}
	}
}