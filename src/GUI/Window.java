package GUI;

import javax.swing.*;
import java.awt.*;
import File.Cell;
import File.Sheet;
import File.SpreadSheetFile;

@SuppressWarnings("serial")
public class Window extends JFrame {
	
	private JToolBar tbMain;
	private FormuleBalk formule;
	private JTabbedPane tabbedPane;
	private SStatusBar statusBar;
	
	private static SpreadSheetFile newFile;
	
	/**
	 * Constructor for the GUI.
	 * @param title is the title of the window.
	 * @throws HeadlessException
	 */
	public Window(String title) throws HeadlessException {
		super(title);
		
		setSize(800, 450);
		setLocationRelativeTo(null);
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		setVisible(true);
		
		setJMenuBar(new SMenuBar(this));
		
		tbMain = new SToolbar(this);
		tabbedPane = new JTabbedPane();
		statusBar = new SStatusBar(this);
		formule = new FormuleBalk();
		tbMain.add(formule);
		
		Container container = getContentPane();
		container.setLayout(new BorderLayout());
		
		container.add(tbMain, BorderLayout.PAGE_START);
		container.add(statusBar, BorderLayout.PAGE_END);
		container.add(tabbedPane, BorderLayout.CENTER);
		tabbedPane.setTabPlacement(JTabbedPane.BOTTOM);
		
		newFile = new SpreadSheetFile();
		createSheet();
	}
	
	public void createSheet() {
		Sheet newSheet = newFile.newSheet("Sheet" + (newFile.countSheets() + 1));
		System.out.println("countSheets() = " + newFile.countSheets());
		
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
		sheet.createCell("=B1+E1", 2, 0);

		Cell X = sheet.createCell("COUNT:", 3, 0);
		X.setbColor(new Color(180, 180, 255));
		X.setBold(true);
		sheet.createCell("=COUNT(A2:T200)", 4, 0);
		sheet.createCell("COUNTIF>50:", 6, 0);
		sheet.createCell("=COUNTIF(A2:T200;\">=\"&50))", 7, 0);
		sheet.createCell("SUMIF>50:", 9, 0);
		sheet.createCell("=SUMIF(A2:T200,\">\"&50)", 10, 0);

		for (int i = 0; i < 20; i++) {
			for (int j = 1; j < 200; j++) {
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
	
	public JLabel getStatusBar() {
		return statusBar;
	}
	
	public void setStatusBarVisibility(boolean visible) {
		statusBar.setVisible(visible);
	}
	
	
	public static void main(String[] args) {
		new Window("Sandwich Spreadsheet");
	}
}