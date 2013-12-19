package GUI;

import javax.swing.*;

import java.awt.*;

import File.Sheet;
import File.SpreadSheetFile;

@SuppressWarnings("serial")
public class Window extends JFrame {
	
	private JToolBar tbMain;
	private FormuleBalk formule;
	private JTabbedPane tabbedPane;
	
	private static SpreadSheetFile newFile;
	
	/**
	 * Constructor for the GUI.
	 * @param title is the title of the window.
	 * @throws HeadlessException
	 */
	public Window(String title) throws HeadlessException {
		super(title);

		setSize(700, 500);
		setLocationRelativeTo(null);
		setJMenuBar(new SMenuBar(this));
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		tbMain = new SToolbar();
		tabbedPane = new JTabbedPane();
		formule = new FormuleBalk();
		tbMain.add(formule);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		container.add(tbMain, BorderLayout.PAGE_START);
		
		add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.setTabPlacement(JTabbedPane.BOTTOM);
		
		newFile = new SpreadSheetFile();
		createSheet();
	}
	
	public void createSheet() {
		System.out.println("countSheets() = " + newFile.countSheets());
		Sheet newSheet = newFile.newSheet("Sheet" + (newFile.countSheets() + 1));
		
		fillSheet(newSheet);
		
		JTable table = new STable(newSheet, formule);
		
		Box box = Box.createVerticalBox();
		box.add(table.getTableHeader());
		JScrollPane scrollPane = new JScrollPane(table);
		scrollPane.setPreferredSize(new Dimension(700,500));
		box.add(scrollPane);
		
		tabbedPane.addTab(newSheet.getSheetName(), box);
	}
	
	private void fillSheet(Sheet sheet) {
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

	public JTabbedPane getTabbedPane() {
		return tabbedPane;
	}

	public void setTabPlacement(int setting) {
		tabbedPane.setTabPlacement(setting);
	}
	
	
	
	public static void main(String[] args) {
		new Window("Sandwich Spreadsheet");
	}
}