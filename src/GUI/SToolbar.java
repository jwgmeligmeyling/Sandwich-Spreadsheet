package GUI;

import java.awt.Dimension;
import java.awt.event.ActionEvent;
import javax.swing.AbstractAction;
import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JOptionPane;
import javax.swing.JToggleButton;
import javax.swing.JToolBar;

@SuppressWarnings("serial")
public class SToolbar extends JToolBar {
	
	private static ImageIcon icoNew = new ImageIcon("img/new.png", "New");
	private static ImageIcon icoOpen = new ImageIcon("img/open.png", "Open");
	private static ImageIcon icoSave = new ImageIcon("img/save.png", "Save");
	private static ImageIcon icoBold = new ImageIcon("img/text-bold.png", "Bold");
	private static ImageIcon icoItalic = new ImageIcon("img/text-italic.png", "Italic");
	private static ImageIcon icoUnderlined = new ImageIcon("img/text-underlined.png", "Underlined");
	private static ImageIcon icoPrint = new ImageIcon("img/print.png", "Print");
	
	private int toolbarHeight = 25;
	
	public SToolbar() {
		super();
		setFloatable(false);
		
		add(new ToolBarButton(fileNew));
		add(new ToolBarButton(fileOpen));
		add(new ToolBarButton(fileSave));
		add(new ToolBarButton(filePrint));
		addSeparator();
		
		add(new JButton("F Color"));
		add(new JButton("B Color"));
		addSeparator();
		
		add(new ToolBarToggleButton(bold));
		add(new ToolBarToggleButton(italic));
		add(new ToolBarToggleButton(underline));
		addSeparator();
	}
	
	public int getToolbarHeight() {
		return toolbarHeight;
	}

	public void setToolbarHeight(int toolbarHeight) {
		this.toolbarHeight = toolbarHeight;
	}

	private AbstractAction fileNew = new AbstractAction(null, icoNew) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>New";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	private AbstractAction fileOpen = new AbstractAction(null, icoOpen) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Open";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	private AbstractAction fileSave = new AbstractAction(null, icoSave) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Save";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	private AbstractAction filePrint = new AbstractAction(null, icoPrint) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "File>Print";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	private AbstractAction bold = new AbstractAction(null, icoBold) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Bold";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	private AbstractAction italic = new AbstractAction(null, icoItalic) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Italic";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	private AbstractAction underline = new AbstractAction(null, icoUnderlined) {

		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Underline";
			JOptionPane.showMessageDialog(null, st);			
		}
		
	};
	
	public class ToolBarButton extends JButton {
		public ToolBarButton(Action action) {
			super(action);
		}
		
		@Deprecated
		public ToolBarButton(String string) {
			super(string);
		}
		
		@Override
		public Dimension getMaximumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);

		}

		@Override
		public Dimension getMinimumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}

		@Override
		public Dimension getPreferredSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}
	}
	
	public class ToolBarToggleButton extends JToggleButton {
		
		ToolBarToggleButton(Action action) {
			super(action);
		}
		
		@Override
		public Dimension getMaximumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);

		}

		@Override
		public Dimension getMinimumSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}

		@Override
		public Dimension getPreferredSize() {
			return new Dimension(toolbarHeight, toolbarHeight);
		}
	}
	
}
