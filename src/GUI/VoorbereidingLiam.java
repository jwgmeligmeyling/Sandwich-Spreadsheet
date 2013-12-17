package GUI;

import javax.swing.*;

import File.Sheet;

import java.awt.*;

@SuppressWarnings("serial")
public class VoorbereidingLiam extends JFrame {
	
	private static JTextField formule;
	private static Sheet sheet;

	public VoorbereidingLiam(String title) throws HeadlessException {
		super(title);
	}
	
	public static class Toolbar extends JToolBar {
		
		public Toolbar() {
			setFloatable(false);
			
			add(new JButton(null, new ImageIcon("img/1387247754_file.png", "New")));
			add(new JButton(null, new ImageIcon("img/1387248351_52.png", "Open")));
			add(new JButton(null, new ImageIcon("img/1387248477_22.png", "Save")));
			addSeparator();
			
			add(new JButton(null, new ImageIcon("img/1387248338_16.png", "Print")));
			addSeparator();
			
			formule = new JTextField();
			add(formule);
		}
		
	}
	
	public static void fillSheet() {
		sheet = new Sheet();
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
				sheet.createCell("=5", i, j);
			}
		}
		
//		sheet.createCell("", 50, 2000);
	}

	public static void main(String[] args) {
		JFrame frame = new VoorbereidingLiam("Sandwich Spreadsheet");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		Container container = frame.getContentPane();
		container.setLayout(new BorderLayout());
		container.add(new Toolbar(), BorderLayout.PAGE_START);
		
		fillSheet();
		
		JTable table = new STable(sheet, formule);
		
		table.setShowVerticalLines(true);
		table.setShowHorizontalLines(true);
		
		Box box = Box.createVerticalBox();
		box.add(table.getTableHeader());
		JScrollPane scrollPane = new JScrollPane(table);
		scrollPane.setPreferredSize(new Dimension(700,500));
		box.add(scrollPane);
		container.add(box, BorderLayout.CENTER);
		
		frame.setLocationRelativeTo(null);
		frame.pack();
		frame.setVisible(true);
	}

}
