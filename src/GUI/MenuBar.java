package GUI;

import javax.swing.JOptionPane;

public class MenuBar{
	
	public static void FileOpen_Click() {
		String st = "File>Open";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void FileSaveAs_Click() {
		String st = "File>Open";
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
	
	public static void InsertGraph_Click() {
		String st = "Insert>Graph";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void InsertFunction_Click() {
		String st = "Insert>Function";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void InsertWorksheet_Click() {
		String st = "Insert>Worksheet";
		JOptionPane.showMessageDialog(null, st);
	}
}
