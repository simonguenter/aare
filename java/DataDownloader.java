package ch.dvbern.nilplus;/*
 * Copyright © 2010 DV Bern AG, Switzerland
 *
 * Das vorliegende Dokument, einschliesslich aller seiner Teile, ist urheberrechtlich
 * geschuetzt. Jede Verwertung ist ohne Zustimmung der DV Bern AG unzulaessig. Dies gilt
 * insbesondere fuer Vervielfaeltigungen, die Einspeicherung und Verarbeitung in
 * elektronischer Form. Wird das Dokument einem Kunden im Rahmen der Projektarbeit zur
 * Ansicht uebergeben ist jede weitere Verteilung durch den Kunden an Dritte untersagt.
 *
 * 1$
 */

import java.io.BufferedReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Date;

public class DataDownloader {

    public static final String DATA_FILE_HYDRO = "c:/temp/aare/data_hydro_";
    public static final String DATA_FILE_SMN = "c:/temp/aare/data_smn_";
    public static final String LOCATIONS[] = new String[]{"brienz", "interlaken", "thun", "B\\u00e4rn", "hagneck", "biel", "brugg"};
    public static final String LOCATIONS2[] = new String[]{"THU", "INT", "BRZ", "BAN", "BER", "MSK",};
    public static SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy_MM_dd");


    public static void main(String[] args) throws Exception {
        // downloading
        for (String location : LOCATIONS) {
            BufferedReader br = null;
            FileWriter fileWriter = null;
            String filenameLocation = location;
            if (filenameLocation.equals("B\\u00e4rn")) {
                filenameLocation = "Bern";
            }
            String filename = DATA_FILE_HYDRO + filenameLocation + "_" + dateFormat.format(new Date());
            saveFromUrl(location, br, fileWriter, filename, "http://aareguru.existenz.ch/v2018/current?city=");
        }

        for (String location : LOCATIONS2) {
            BufferedReader br = null;
            FileWriter fileWriter = null;

            String filename = DATA_FILE_SMN + location + "_" + dateFormat.format(new Date());
            saveFromUrl(location, br, fileWriter, filename, "http://api.existenz.ch/apiv1/smn/daterange?startdate=-24%20hours&enddate=now&locations=");
        }
    }

    private static void saveFromUrl(String location, BufferedReader br, FileWriter fileWriter, String filename, String urlString) throws IOException {
        try {

            URL url = new URL(urlString + location);
            br = new BufferedReader(new InputStreamReader(url.openStream()));

            String line;

            StringBuilder sb = new StringBuilder();

            while ((line = br.readLine()) != null) {

                sb.append(line);
                sb.append(System.lineSeparator());
            }
            fileWriter = new FileWriter(filename);
            fileWriter.write(sb.toString());
            fileWriter.close();
            //System.out.println(sb);
        } finally {

            if (br != null) {
                br.close();
            }
            if (fileWriter != null) {
                fileWriter.close();
            }
        }
    }
}


