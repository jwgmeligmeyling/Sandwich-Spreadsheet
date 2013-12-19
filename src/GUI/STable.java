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
	
	
	public STable(final Sheet sheet, FormuleBalk formule) {
		super(new AbstractTableModel() {
			public String getColumnName(int column) {
				if (column == 0) {
					return "";
				}
				return sheet.getColumnLetter(column - 1);
			}

			public int getRowCount() {
				return sheet.getRowCount();
			}

			public int getColumnCount() {
				return sheet.getColumnCount() + 1;
			}

			public Object getValueAt(int row, int col) {
				if (col == 0) {
					return row + 1;
				}
				return sheet.getCellAt(col - 1, row).getValue();
			}

			public boolean isCellEditable(int row, int column) {
				return column != 0;
			}

			public void setValueAt(Object value, int row, int col) {
				sheet.getCellAt(col - 1, row).setInput(value.toString());
				fireTableCellUpdated(row, col);
			}
		}, null, null);

		SelectionHandler handler = new SelectionHandler();
		getSelectionModel().addListSelectionListener(handler);
		getColumnModel().getSelectionModel().addListSelectionListener(handler);

		setSelectionMode(ListSelectionModel.SINGLE_INTERVAL_SELECTION);
		setRowSelectionAllowed(true);
		setColumnSelectionAllowed(true);
		setCellSelectionEnabled(true);
		
		setGridColor(DEFAULT_GRID_COLOR);
		setSelectionBackground(DEFAULT_SELECTION_COLOR);
		setSelectionForeground(DEFAULT_SELECTION_TEXT);

		getColumnModel().getColumn(0).setPreferredWidth(100);
		getColumnModel().getColumn(0).setCellRenderer(new TableCellRenderer() {

			@Override
			public Component getTableCellRendererComponent(JTable x,
					Object value, boolean isSelected, boolean hasFocus,
					int row, int column) {
				boolean selected = getSelectionModel().isSelectedIndex(row);
				Component component = x
						.getTableHeader()
						.getDefaultRenderer()
						.getTableCellRendererComponent(x, value, false, false,
								-1, -2);
				((JLabel) component).setHorizontalAlignment(JLabel.CENTER);
				if (selected) {
					component
							.setFont(component.getFont().deriveFont(Font.BOLD));
				} else {
					component.setFont(component.getFont()
							.deriveFont(Font.PLAIN));
				}
				return component;
			}
		});

		this.sheet = sheet;
		this.formuleBalk = formule;
	}

	@Override
	public TableCellEditor getCellEditor(int row, int column) {
		currentEditor = new JTextField();
		formuleBalk.setCurrentTable(table);
		formuleBalk.setCurrentEditor(currentEditor);
		return new CustomTableCellEditor();
	}

	public class CustomTableCellEditor extends DefaultCellEditor implements
			TableCellEditor {
		String value;

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

	class SelectionHandler implements ListSelectionListener {

		@Override
		public void valueChanged(ListSelectionEvent e) {
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
