import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

/**
 * @version <b>20-11-2013</b>	Menu aangemaakt met links naar ActionListeners<br>
 * 								Eerste versie van gridview aangemaakt
 * @author Maarten Flikkema
 */
public class GridView extends JFrame implements ActionListener {
	
	/**
	 * Declaratie van controls.
	 */
	private JTextField[][] grid;				// names the grid of buttons
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
				private JMenuItem jmiInsGraph = new JMenuItem("Make Graph");
				
			private JMenu menuHelp = new JMenu("Help");
				private JMenuItem jmiHelp = new JMenuItem("Help");
				
			private JTabbedPane tabs = new JTabbedPane();
			
			private 
	
	
/**
 * Constructor voor Frame
 * @param width		
 * @param length	
 */
	public GridView(int width, int length) {	
		
		this.setLayout(new GridLayout(width, length));
		
		setJMenuBar(jmb);
		
		jmb.add(menuFile);
			menuFile.setMnemonic('f');
			menuFile.add(jmiOpen);	jmiOpen.setMnemonic('o');
			menuFile.add(jmiSaveAs);	jmiSaveAs.setMnemonic('s');
			menuFile.add(jmiExit);	jmiExit.setMnemonic('s');
			
		jmb.add(menuEdit);
			menuEdit.setMnemonic('e');
			menuEdit.add(jmiUndo);	jmiUndo.setMnemonic('u');
			menuEdit.add(jmiRedo);	jmiRedo.setMnemonic('r');
			menuEdit.add(jmiCut);	jmiCut.setMnemonic('x');
			menuEdit.add(jmiCopy);	jmiCopy.setMnemonic('c');
			menuEdit.add(jmiPaste);	jmiPaste.setMnemonic('v');
			
		jmb.add(menuView);
			menuView.setMnemonic('v');
			menuView.add(jmiZoom);		jmiZoom.setMnemonic('z');
			menuView.add(jmiStatusBar);	jmiStatusBar.setMnemonic('s');
			
		jmb.add(menuInsert);
			menuInsert.setMnemonic('i');
			menuInsert.add(jmiInsGraph);	jmiInsGraph.setMnemonic('g');
			menuInsert.add(jmiInsFunction);	jmiInsFunction.setMnemonic('f');
			
		jmb.add(menuHelp);
			menuHelp.setMnemonic('h');
			menuHelp.add(jmiHelp);	jmiHelp.setMnemonic('h');
			
		
		grid = new JTextField[width][length];							// allocate the size of grid
		for (int y = 0; y < length; y++) {
			for (int x = 0; x < width; x++) {
				grid[x][y] = new JTextField("(" + x + "," + y + ")");	// creates new button
				add(grid[x][y]);										// adds button to grid
			}
		}
		
		// Add ActionListeners±
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
		
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		this.pack();		// sets appropriate size for frame
		setVisible(true);	// makes frame visible
	}
	
	@Override
	public void actionPerformed(ActionEvent e) {
		
		// Menu File
		if (e.getSource() == jmiOpen) {
			
		}
		if (e.getSource() == jmiSaveAs) {
			
		}
		if (e.getSource() == jmiExit) {
			int answer = JOptionPane.showConfirmDialog(this, "Are you sure you want to quit", "Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.QUESTION_MESSAGE);
			if (answer == JOptionPane.YES_OPTION) { System.exit(0);	}
		}
		
		// Menu Edit
		if (e.getSource() == jmiUndo) {
			
		}
		if (e.getSource() == jmiRedo) {
			
		}
		if (e.getSource() == jmiCut) {
			
		}
		if (e.getSource() == jmiCopy) {
			
		}
		if (e.getSource() == jmiPaste) {
			
		}
			
		// Menu View
		if (e.getSource() == jmiZoom) {
			
		}
		
		// Menu Insert
		if (e.getSource() == jmiInsGraph) {
			
		}
		if (e.getSource() == jmiInsFunction) {
			
		}
	}
}