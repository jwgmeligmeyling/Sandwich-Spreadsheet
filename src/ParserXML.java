import java.io.File;
import javax.xml.parsers.*;
import org.w3c.dom.*;

public class ParserXML {
	
	// Maarten, als je een dergelijk pad gebruikt, werkt hij bij iedereen, toch?
	static String FILE_ADDRESS = "bron.xml";
	
	public static void main(String args[]) {
		
		try {
			File cells = new File(FILE_ADDRESS);
			DocumentBuilderFactory dbFactory = DocumentBuilderFactory.newInstance();
			DocumentBuilder dBuilder = dbFactory.newDocumentBuilder();
			Document doc = dBuilder.parse(cells);
			doc.getDocumentElement().normalize();
			
			System.out.println("root of xml file=:" + doc.getDocumentElement().getNodeName());
			NodeList nodes = doc.getElementsByTagName("SPREADSHEET");
			
			System.out.println();
			
			for (int i = 0; i < nodes.getLength(); i++) {
				Node node = nodes.item(i);
				
				if (node.getNodeType() == Node.ELEMENT_NODE) {
					Element element = (Element)node;
					System.out.println("Cel: " + getValue("CELL", element));
				}
			}
			
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}
	
	private static String getValue(String tag, Element element) {
		NodeList nodes = element.getElementsByTagName(tag).item(0).getChildNodes();
		Node node = (Node)nodes.item(0);
		return node.getNodeValue();

	}
}

/*
 * Output:
 * 
 * root of xml file stocks ========================== Stock Symbol: Citibank
 * Stock Price: 100 Stock Quantity: 1000 Stock Symbol: Axis bank Stock Price: 90
 * Stock Quantity: 2000
 */