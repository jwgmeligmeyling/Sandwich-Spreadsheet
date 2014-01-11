package Parser;

import java.util.Scanner;

import File.Sheet;

public class ParseTyper {
	
	/**
	 * Run and type!
	 * @param args
	 */
	public static void main(String[] args) {
		System.out.println("Start to type expressions!");
		System.out.println();
		System.out.println("Some examples:");
		System.out.println("=5+2*3");
		
		Sheet sheet = new Sheet();
		sheet.createCell("bliep", 0, 0);
		sheet.createCell("5", 0, 1);
		sheet.createCell("=5", 0, 2);
		sheet.createCell("=5*2", 1, 0);
		sheet.createCell("=2+2*3", 1, 1);
		sheet.createCell("=SUM(5,3)", 1, 2);
		sheet.init();
		
		Scanner sc = new Scanner(System.in);
		sc.useDelimiter("\n");
		
		while(sc.hasNext()) {
			try {
				System.out.println(Parser.parse(sheet, sc.next()));
			} catch ( Exception e ) {
				e.printStackTrace();
			}
		}
		
		sc.close();
	}

}
