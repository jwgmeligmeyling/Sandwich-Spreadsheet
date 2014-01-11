package Zandbak;

import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;

public class Events {
	
	public static void FileOpen_Click() {
		String st = "File>Open";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void FileSave_Click() {
		String st = "File>Save";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void FileSaveAs_Click() {
		String st = "File>SaveAs";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void FileExit_Click() {
		int answer = JOptionPane.showConfirmDialog(null, "Are you sure you want to quit?", "Please confirm", JOptionPane.YES_NO_OPTION);
		if (answer == JOptionPane.YES_OPTION) { System.exit(0);	}
	}
	
	public static void EditUndo_Click() {
		String st = "Edit>Undo";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void EditRedo_Click() {
		String st = "Edit>Redo";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void EditCut_Click() {
		String st = "Edit>Cut";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void EditCopy_Click() {
		String st = "Edit>Copy";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void EditPaste_Click() {
		String st = "Edit>Paste";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void ViewZoom_Click() {
		String st = "View>Zoom";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void ViewStatusBar_Click() {
		String st = "View>StatusBar";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void ViewTabsTop_Click(JTabbedPane tabPane) {
		tabPane.setTabPlacement(JTabbedPane.TOP);
	}
	
	public static void ViewTabsBottom_Click(JTabbedPane tabPane) {
		tabPane.setTabPlacement(JTabbedPane.BOTTOM);
	}
	
	public static void InsertGraph_Click() {
		String st = "Insert>Graph";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void InsertFunction_Click() {
		String st = "Insert>Function";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void InsertWorksheet_Click() {
	//	Window.createSheet();
		//String st = "Insert>Worksheet";
		//JOptionPane.showMessageDialog(null, st);
	}
	
	public static void HelpHelp_Click() {
		String st = "Help>Help";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void HelpAbout_Click() {
		String st = "Help>About";
		JOptionPane.showMessageDialog(null, st);
	}
	
	
	public static void tbMainNew_Click() {
		String st = "ToolBar(Main)>New";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainOpen_Click() {
		String st = "ToolBar(Main)>Open";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainSave_Click() {
		String st = "ToolBar(Main)>Save";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainPrint_Click() {
		String st = "ToolBar(Main)>Print";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainFColor_Click() {
		String st = "ToolBar(Main)>FColor";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainBColor_Click() {
		String st = "ToolBar(Main)>BColor";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainBold_Click() {
		String st = "ToolBar(Main)>Bold";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainItalic_Click() {
		String st = "ToolBar(Main)>Italic";
		JOptionPane.showMessageDialog(null, st);
	}

	public static void tbMainUnderlined_Click() {
		String st = "ToolBar(Main)>Underlined";
		JOptionPane.showMessageDialog(null, st);
	}
}