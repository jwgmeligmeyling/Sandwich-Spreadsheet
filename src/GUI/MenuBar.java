package GUI;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JOptionPane;

public class MenuBar implements ActionListener{
	
	public static void FileOpen_Click() {
		String st = "File>Open";
		JOptionPane.showMessageDialog(null, st);
	}
	
	public static void FileSaveAs_Click() {
		
	}
	
	public static void FileExit_Click() {
		int answer = JOptionPane.showConfirmDialog(null, "Are you sure you want to quit?", "Please confirm", JOptionPane.YES_NO_OPTION);
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
	
	public static void InsertWorksheet_Click() {
		
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		// TODO Auto-generated method stub
		
	}
}
