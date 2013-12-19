package GUI;

import java.awt.Color;
import java.awt.Component;
import java.awt.Font;

import javax.swing.DefaultCellEditor;
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.border.LineBorder;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.AbstractTableModel;
import javax.swing.table.JTableHeader;
import javax.swing.table.TableCellEditor;
import javax.swing.table.TableCellRenderer;

import File.Cell;
import File.Sheet;
import File.Sheet.Range;

@SuppressWarnings("serial")
public class STable extends JTable {
	
	private final Sheet sheet;
	private final FormuleBalk formuleBalk;
	private final STable table = this;
	
	private Range selectedRange;
	private JTextField currentEditor;

	private static final Color DEFAULT_GRID_COLOR = new Color(206,206,206);
	private static final Color DEFAULT_SELECTION_COLOR = new Color(190, 220, 255);
	private static final Color DEFAULT_SELECTION_TEXT = new Color(0,0,0);
	
	public STable(Sheet sheet, FormuleBalk formule) {
		super(new TableModel(sheet), null, null);
		this.sheet = sheet;
		this.formuleBalk = formule;
		
		SelectionHandler handler = new SelectionHandler();
		getSelectionModel().addListSelectionListener(handler);
		getColumnModel().getSelectionModel().addListSelectionListener(handler);

		setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		setCellSelectionEnabled(true);
		gridColor = DEFAULT_GRID_COLOR;
		selectionBackground = DEFAULT_SELECTION_COLOR;
		selectionForeground = DEFAULT_SELECTION_TEXT;
		autoResizeMode = AUTO_RESIZE_OFF;
		
		JTableHeader header = getTableHeader();
		header.setDefaultRenderer(new HeaderNameRenderer(header.getDefaultRenderer()));
		
		getColumnModel().getColumn(0).setPreferredWidth(50);
		getColumnModel().getColumn(0).setCellRenderer(new RowNumberRenderer());
	}

	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		currentEditor = new JTextField();
		formuleBalk.setCurrentTable(table);
		formuleBalk.setCurrentEditor(currentEditor);
		return new CustomTableCellEditor();
	}

	/**
	 * The custom TableCellEditor binds the <code>Sheet</code> class to this
	 * current <code>STable</code> instance.
	 * 
	 * @author Jan-Willem Gmelig Meyling, Liam Clark
	 * 
	 */
	private class CustomTableCellEditor extends DefaultCellEditor {
		private String value;

		/**
		 * Constructor for the CustomTableCellEditor. Creates a new table cell
		 * editor with the JTextField stored in the currentEditor variable.
		 */
		public CustomTableCellEditor() {
			super(currentEditor);
			getComponent().setName("Table.editor");
		}

		@Override
		public boolean stopCellEditing() {
			value = (String) super.getCellEditorValue();
			return super.stopCellEditing();
		}

		@Override
		public Component getTableCellEditorComponent(JTable table,
				Object value, boolean isSelected, int row, int column) {
			Cell cell = sheet.getCellAt(column - 1, row);
			Object input = cell.getInput();
			this.value = null;
			((JComponent) getComponent())
					.setBorder(new LineBorder(Color.black));
			return super.getTableCellEditorComponent(table, input, isSelected,
					row, column);
		}

		@Override
		public Object getCellEditorValue() {
			return value;
		}
	}

	/**
	 * The custom TableCellEditor binds the <code>Sheet</code> class to this
	 * current <code>STable</code> instance.
	 * 
	 * @author Jan-Willem Gmelig Meyling, Liam Clark
	 * 
	 */
	private static class TableModel extends AbstractTableModel {
	
		private final Sheet sheet;
		
		/**
		 * Constructor for the TableModel, sets a sheet variable.
		 * @param sheet
		 */
		private TableModel(Sheet sheet) {
			this.sheet = sheet;
		}
		
		@Override
		public String getColumnName(int column) {
			if (column == 0) {
				return "";
			}
			return sheet.getColumnLetter(column - 1);
		}
		
		@Override
		public int getRowCount() {
			return sheet.getRowCount();
		}
	
		@Override
		public int getColumnCount() {
			return sheet.getColumnCount() + 1;
		}
	
		@Override
		public Object getValueAt(int row, int col) {
			if (col == 0) {
				return row + 1;
			}
			return sheet.getCellAt(col - 1, row).getValue();
		}
	
		@Override
		public boolean isCellEditable(int row, int column) {
			return column != 0;
		}
		
		@Override
		public void setValueAt(Object value, int row, int col) {
			sheet.getCellAt(col - 1, row).setInput(value.toString());
			fireTableCellUpdated(row, col);
		}
		
	}

	/**
	 * Renderer for the rowNumbers
	 * @author Jan-willem Gmelig Meyling, Liam Clark
	 *
	 */
	private class RowNumberRenderer implements TableCellRenderer {
		
		@Override
		public Component getTableCellRendererComponent(JTable x,
				Object value, boolean isSelected, boolean hasFocus,
				int row, int column) {
			boolean selected = getSelectionModel().isSelectedIndex(row);
			Component component = x
					.getTableHeader()
					.getDefaultRenderer()
					.getTableCellRendererComponent(x, value, false, false, -1, -2);
			((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
			if (selected) {
				component.setFont(component.getFont().deriveFont(Font.BOLD));
			} else {
				component.setFont(component.getFont().deriveFont(Font.PLAIN));
			}
			return component;
		}
		
	}
	
	/**
	 * Override the header name renderer
	 *
	 */
	private class HeaderNameRenderer implements TableCellRenderer {

		 private TableCellRenderer delegate;

		 /**
		  * Constructor for the header name renderer
		  * @param delegate
		  */
		 public HeaderNameRenderer(TableCellRenderer delegate) {
		     this.delegate = delegate;
		 } 

		@Override
		public Component getTableCellRendererComponent(JTable table,
				Object value, boolean isSelected, boolean hasFocus, int row,
				int column) {
			Component component = delegate.getTableCellRendererComponent(table,
					value, false, false, row, column);
			if (isSelected(column)) {
				component.setFont(component.getFont().deriveFont(Font.BOLD));
			}
			return component;
		}
		
		/**
		 * Determine if a cell in the current column is selected
		 * @param column
		 * @return
		 */
		private boolean isSelected(int column) {
			for ( int i : getSelectedColumns() ) {
				if ( i == column ) {
					return true;
				}
			}
			return false;
		}
		
	}

	/**
	 * The selection handler listens for new selected Ranges
	 * @author Liam Clark
	 *
	 */
	private class SelectionHandler implements ListSelectionListener {

		@Override
		public void valueChanged(ListSelectionEvent e) {
			tableHeader.repaint();
			
			if (e.getValueIsAdjusting()) {
				formuleBalk.setText("");
				return;
			}
			int[] selectedColumns = getSelectedColumns();
			int[] selectedRows = getSelectedRows();

			if (selectedColumns != null & selectedRows != null) {
				int selectedColumnsCount = selectedColumns.length;
				int selectedRowsCount = selectedRows.length;

				if (selectedColumnsCount == 0 || selectedRowsCount == 0) {
					return;
				}

				int colLeft = selectedColumns[0] - 1;
				int rowUp = selectedRows[0];
				int colRight = selectedColumns[selectedColumnsCount - 1] - 1;
				int rowDown = selectedRows[selectedRowsCount - 1];
				
				if (colLeft == -1) {
					return; // First column is reserved for row index
				}

				Range range = sheet.getRange(colLeft, rowUp, colRight, rowDown);
				
				if (!range.equals(selectedRange)) {
					selectedRange = range;
					System.out.println("Selected range: " + range.toString());
				}
				
				if (range.isSingleCell()) {
					Cell selectedCell = range.firstCell();
					formuleBalk.setText(selectedCell.getInput());
				}

			}
		}

	}
}
