package GUI;

import java.awt.GridBagLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.*;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.TableModel;

/**
 * @author Maarten Flikkema
 * @version <b>20-11-2013</b>	Menu aangemaakt met links naar ActionListeners<br>
 * 								Eerste versie van gridview aangemaakt
 * @version <b>versie 2</b>		...
 * 
 */
@SuppressWarnings("serial")
public class Window extends JFrame implements ActionListener {
	
	public static void main(String[] args) {
		new Window();
	}
	
	
	/**
	 * Declaratie van controls.
	 */
	//private JTextField[][] grid;				// names the grid of buttons
	
	private JMenuBar jmb = new JMenuBar();
	
	private JMenu menuFile = new JMenu("File");
		private JMenuItem jmiOpen = new JMenuItem("Open");
		private JMenuItem jmiSaveAs = new JMenuItem("Save As");
		private JMenuItem jmiExit = new JMenuItem("Exit");
		
	private JMenu menuEdit = new JMenu("Edit");
		private JMenuItem jmiUndo = new JMenuItem("Undo");
		private JMenuItem jmiRedo = new JMenuItem("Redo");
		private JMenuItem jmiCut = new JMenuItem("Cut");
		private JMenuItem jmiCopy = new JMenuItem("Copy");
		private JMenuItem jmiPaste = new JMenuItem("Paste");
		
	private JMenu menuView = new JMenu("View");
		private JMenuItem jmiZoom = new JMenuItem("Zoom in/out");
		private JMenuItem jmiStatusBar = new JMenuItem("Show/hide Status Bar");
		
	private JMenu menuInsert = new JMenu("Insert");
		private JMenuItem jmiInsFunction = new JMenuItem("Insert Function");
		private JMenuItem jmiInsGraph = new JMenuItem("Create new Graph");
		private JMenuItem jmiInsSheet = new JMenuItem("Create new Sheet");
		
	private JMenu menuHelp = new JMenu("Help");
		private JMenuItem jmiHelp = new JMenuItem("Help");
		
	private JToolBar tbMain = new JToolBar();
		private JButton jbFColor = new JButton("F color");
		private JButton jbBColor = new JButton("B color");
		private JButton jbBold = new JButton("Bold");
		private JButton jbItalic = new JButton("Italic");
		// later vervangen door image icon       ^^^^
	
	JTabbedPane tabs = new JTabbedPane();
	
	private JButton jbTempKnop = new JButton("Test");
	
	TableModel dataModel = new AbstractTableModel() {
		public int getColumnCount() { return 10; }
		public int getRowCount() { return 10; }
		public Object getValueAt(int row, int col) { return new Integer(row * col); }
	};
	
	private JTable table = new JTable(dataModel);
	private JScrollPane scrollpane = new JScrollPane(table);
	
	private JLabel statusBar = new JLabel("Dit is de statusbar");
	// Zichtbaar danwel onzichtbaar maken afhankelijk van keuze van gebruiker in menu: "View>Statusbar"
	
/**
 * Constructor voor Frame
 * 
 */
	public Window() {	
		
		int AANTAL_KOLOMMEN = 20;
		int AANTAL_RIJEN = 20;
		
		//this.setLayout(new GridLayout(AANTAL_KOLOMMEN, AANTAL_RIJEN));
		
		this.setLayout(new GridBagLayout());
		
		setJMenuBar(jmb);
		
		jmb.add(menuFile);
			menuFile.setMnemonic('f');
			menuFile.add(jmiOpen);		jmiOpen.setMnemonic('o');
			menuFile.add(jmiSaveAs);	jmiSaveAs.setMnemonic('s');
			menuFile.add(jmiExit);		jmiExit.setMnemonic('s');
			
		jmb.add(menuEdit);
			menuEdit.setMnemonic('e');
			menuEdit.add(jmiUndo);		jmiUndo.setMnemonic('u');
			menuEdit.add(jmiRedo);		jmiRedo.setMnemonic('r');
			menuEdit.add(jmiCut);		jmiCut.setMnemonic('x');
			menuEdit.add(jmiCopy);		jmiCopy.setMnemonic('c');
			menuEdit.add(jmiPaste);		jmiPaste.setMnemonic('v');
			
		jmb.add(menuView);
			menuView.setMnemonic('v');
			menuView.add(jmiZoom);		jmiZoom.setMnemonic('z');
			menuView.add(jmiStatusBar);	jmiStatusBar.setMnemonic('s');
			
		jmb.add(menuInsert);
			menuInsert.setMnemonic('i');
			menuInsert.add(jmiInsGraph);	jmiInsGraph.setMnemonic('g');
			menuInsert.add(jmiInsFunction);	jmiInsFunction.setMnemonic('f');
			menuInsert.add(jmiInsSheet);	jmiInsSheet.setMnemonic('s');
			
		jmb.add(menuHelp);
			menuHelp.setMnemonic('h');
			menuHelp.add(jmiHelp);	jmiHelp.setMnemonic('h');
		
		add(tbMain);
			tbMain.add(jbFColor);
			tbMain.add(jbBColor);
			tbMain.add(jbBold);
			tbMain.add(jbItalic);
			
		//tabs.setLayout(new GridLayout(AANTAL_KOLOMMEN, AANTAL_RIJEN));
		add(tabs);
		tabs.addTab("Spreadsheet", table);
		
		// Add ActionListeners
		jmiOpen.addActionListener(this);
		jmiSaveAs.addActionListener(this);
		jmiExit.addActionListener(this);
		jmiUndo.addActionListener(this);
		jmiRedo.addActionListener(this);
		jmiCut.addActionListener(this);
		jmiCopy.addActionListener(this);
		jmiPaste.addActionListener(this);
		jmiZoom.addActionListener(this);
		jmiInsGraph.addActionListener(this);
		jmiInsFunction.addActionListener(this);
		jmiInsSheet.addActionListener(this);
		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.pack();		// sets appropriate size for frame
		setVisible(true);	// makes frame visible
	}
	
	
	@Override
	public void actionPerformed(ActionEvent e) {	
		
		
		// Menu File
		if (e.getSource() == jmiOpen) {			MenuBar.FileOpen_Click(); }
		if (e.getSource() == jmiSaveAs) {		MenuBar.FileSaveAs_Click(); }
		if (e.getSource() == jmiExit) {			MenuBar.FileExit_Click(); }
		
		// Menu Edit
		if (e.getSource() == jmiUndo) {			MenuBar.EditUndo_Click(); }
		if (e.getSource() == jmiRedo) {			MenuBar.EditRedo_Click(); }
		if (e.getSource() == jmiCut) {			MenuBar.EditCut_Click(); }
		if (e.getSource() == jmiCopy) {			MenuBar.EditCopy_Click(); }
		if (e.getSource() == jmiPaste) {		MenuBar.EditPaste_Click(); }
		
		// Menu View
		if (e.getSource() == jmiZoom) {			MenuBar.ViewZoom_Click(); }
		
		// Menu Insert
		if (e.getSource() == jmiInsGraph) {		MenuBar.InsertGraph_Click(); }
		if (e.getSource() == jmiInsFunction) {	MenuBar.InsertFunction_Click(); }
		if (e.getSource() == jmiInsSheet) {		MenuBar.InsertWorksheet_Click(); }
		
		
	}
}