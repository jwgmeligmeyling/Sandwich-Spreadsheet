package Events;

import javax.swing.JOptionPane;

public class MenuBar {
	
	public static void FileOpen_Click() {
		
	}
	
	public static void FileSaveAs_Click() {
		
	}
	
	public static void FileExit_Click() {
		int answer = JOptionPane.showConfirmDialog(null, "Are you sure you want to quit", "Confirmation", JOptionPane.YES_NO_OPTION);
		if (answer == JOptionPane.YES_OPTION) { System.exit(0);	}
	}
	
	public static void EditUndo_Click() {
		
	}
	
	public static void EditRedo_Click() {
		
	}
	
	public static void EditCut_Click() {
		
	}
	
	public static void EditCopy_Click() {
		
	}
	
	public static void EditPaste_Click() {
		
	}
	
	public static void ViewZoom_Click() {
		
	}
	
	public static void InsertGraph_Click() {
		
	}
	
	public static void InsertFunction_Click() {
		
	}
}
