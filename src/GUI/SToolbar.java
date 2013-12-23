package GUI;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Frame;
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
	private static ImageIcon icoBColor = new ImageIcon("img/backcolor.gif", "Background color");
	private static ImageIcon icoFColor = new ImageIcon("img/forecolor.gif", "Text color");

	private static Frame window;
	
	private int toolbarHeight = 25;

	public SToolbar(Frame parent) {
		super();
		window = parent;
		setFloatable(false);

		add(new ToolBarButton(fileNew));
		add(new ToolBarButton(fileOpen));
		add(new ToolBarButton(fileSave));
		add(new ToolBarButton(filePrint));
		addSeparator();
		add(new ToolBarButton(formatFColor));
		add(new ToolBarButton(formatBColor));
		addSeparator();
		add(new ToolBarToggleButton(formatBold));
		add(new ToolBarToggleButton(formatItalic));
		add(new ToolBarToggleButton(formatUnderline));
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

	private AbstractAction formatBold = new AbstractAction(null, icoBold) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Bold";
			JOptionPane.showMessageDialog(null, st);
		}
	};

	private AbstractAction formatItalic = new AbstractAction(null, icoItalic) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Italic";
			JOptionPane.showMessageDialog(null, st);
		}
	};

	private AbstractAction formatUnderline = new AbstractAction(null, icoUnderlined) {
		@Override
		public void actionPerformed(ActionEvent e) {
			String st = "Underline";
			JOptionPane.showMessageDialog(null, st);
		}
	};
	
	private AbstractAction formatBColor = new AbstractAction(null, icoBColor) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Color oldColor = new Color(50, 200, 160);
			new SColorPicker("Choose a background color", oldColor);
			//TODO bColor van selected cells instellen
		}
	};
	
	private AbstractAction formatFColor = new AbstractAction(null, icoFColor) {
		@Override
		public void actionPerformed(ActionEvent e) {
			Color oldColor = new Color(50, 200, 160);
			new SColorPicker("Choose a text color", oldColor);
			//TODO fColor van selected cells instellen
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