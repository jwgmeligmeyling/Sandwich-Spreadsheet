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
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		tbMain = new SToolbar();
		tabbedPane = new JTabbedPane();
		setJMenuBar(new SMenuBar(this));
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
		sheet.createCell("SOM:", 0, 0);
		sheet.createCell("=SUM(A2:T200)", 1, 0);
		sheet.createCell("COUNT:", 3, 0);
		sheet.createCell("=COUNT(A2:T200)", 4, 0);
		sheet.createCell("COUNTIF>50:", 6, 0);
		sheet.createCell("=COUNTIF(A2:T200;\">=\"&50))", 7, 0);
		sheet.createCell("SUMIF>50:", 9, 0);
		sheet.createCell("=SUMIF(A2:T200,\">\"&50)", 10, 0);
		
		for ( int i = 0; i < 20; i++ ) {
			for ( int j = 1; j < 200; j++ ) {
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