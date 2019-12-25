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
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Collection;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import com.google.gson.JsonArray;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParser;

public class DataConverter {

    public static final String DATA_FILE_HYDRO = "c:/temp/aare/data_hydro_";
    public static final String DATA_FILE_SMN = "c:/temp/aare/data_smn_";
    public static final String LOCATIONS[] = new String[]{"Bern","brienz", "interlaken", "thun", "hagneck", "biel", "brugg"};
    public static final String LOCATIONS2[] = new String[]{"BER","THU", "INT", "BRZ", "BAN",  "MSK"};
    public static SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy_MM_dd");

    private static Map<String,Integer> featureToIndexMap = new HashMap<>();

    static {
        featureToIndexMap.put("Bern_flow",0);
        featureToIndexMap.put("Bern_temperature",1);
        featureToIndexMap.put("BER_ff",2);
        featureToIndexMap.put("BER_qnh",3);
        featureToIndexMap.put("BER_rh",4);
        featureToIndexMap.put("BER_rr",5);
        featureToIndexMap.put("BER_ss",6);
        featureToIndexMap.put("BER_td",7);
        featureToIndexMap.put("BER_tt",8);
        featureToIndexMap.put("brienz_flow",9);
        featureToIndexMap.put("brienz_temperature", 10);
        featureToIndexMap.put("interlaken_flow", 11);
        featureToIndexMap.put("interlaken_temperature", 12);
        featureToIndexMap.put("thun_flow", 13);
        featureToIndexMap.put("thun_temperature", 14);
        featureToIndexMap.put("hagneck_flow", 15);
        featureToIndexMap.put("hagneck_temperature", 16);
        featureToIndexMap.put("biel_flow", 17);
        featureToIndexMap.put("biel_temperature", 18);
        featureToIndexMap.put("THU_ff",19);
        featureToIndexMap.put("THU_qnh",20);
        featureToIndexMap.put("THU_rh",21);
        featureToIndexMap.put("THU_rr",22);
        featureToIndexMap.put("THU_ss",23);
        featureToIndexMap.put("THU_td",24);
        featureToIndexMap.put("THU_tt",25);
        featureToIndexMap.put("INT_ff",26);
        featureToIndexMap.put("INT_qnh",27);
        featureToIndexMap.put("INT_rh",28);
        featureToIndexMap.put("INT_rr",29);
        featureToIndexMap.put("INT_ss",30);
        featureToIndexMap.put("INT_td",31);
        featureToIndexMap.put("INT_tt",32);
        featureToIndexMap.put("BRZ_ff",33);
        featureToIndexMap.put("BRZ_qnh",34);
        featureToIndexMap.put("BRZ_rr",35);
        featureToIndexMap.put("BAN_ff_tow",36);
        featureToIndexMap.put("BAN_rh_tow",37);
        featureToIndexMap.put("BAN_ss",38);
        featureToIndexMap.put("BAN_td_tow",39);
        featureToIndexMap.put("BAN_tt_tow",40);
        featureToIndexMap.put("MSK_ff_tow",41);
        featureToIndexMap.put("MSK_rh_tow",42);
        featureToIndexMap.put("MSK_ss",43);
        featureToIndexMap.put("MSK_td_tow",44);
        featureToIndexMap.put("MSK_tt_tow",45);
    }


    public static Date roundToNext10Minutes(Long l) {
        Calendar calendar = Calendar.getInstance();
        Date date = new Date();
        date.setTime(l);
        calendar.setTime(date);

        int unroundedMinutes = calendar.get(Calendar.MINUTE);
        int mod = unroundedMinutes % 10;
        calendar.add(Calendar.MINUTE, 10 - mod);
        calendar.set(Calendar.SECOND, 0);
        calendar.set(Calendar.MILLISECOND, 0);
        return calendar.getTime();
    }


    @Nullable
    public static void getHydroDate(String preample,File file,Map<Date, BigDecimal []> data) throws IOException {
        JsonParser js = new JsonParser();
              JsonElement parse = js.parse(FileUtils.readFileToString(file));

              JsonArray payload = (JsonArray) ((JsonObject) parse).get("aarepast");
              for(Object o : payload) {
                  JsonObject inner = (JsonObject) o;
                  try {
                      Date timestamp = roundToNext10Minutes(1000 * Long.parseLong(inner.get("timestamp").getAsString()));
                      BigDecimal flow = inner.get("flow").getAsBigDecimal();
                      JsonElement temperatureElement = inner.get("temperature");
                      BigDecimal temperature = temperatureElement.getAsBigDecimal();
                      Integer flowIndex = featureToIndexMap.get(preample + "_flow");
                      Integer temperatureIndex = featureToIndexMap.get(preample + "_temperature");
                      if (flowIndex != null) {
                          BigDecimal[] values = data.get(timestamp);
                          if (values == null) {
                              values = new BigDecimal[46];
                              data.put(timestamp, values);
                          }
                          values[flowIndex] = flow;
                          values[temperatureIndex] = temperature;
                      }
                  }
                  catch (Exception e) {
                      System.out.println("error " +  file + " " + o.toString());
                  }
              }
    }


    @Nullable
    public static void getSmsData(String preample,File file,Map<Date, BigDecimal []> data) throws IOException {
        JsonParser js = new JsonParser();
        JsonElement parse = js.parse(FileUtils.readFileToString(file));

        JsonArray payload = (JsonArray) ((JsonObject) parse).get("payload");
        for(Object o : payload) {
            JsonObject inner = (JsonObject) o;
            Date timestamp = roundToNext10Minutes(1000 * Long.parseLong(inner.get("timestamp").getAsString()));
            String parameterName =  inner.get("par").getAsString();
            Integer index = featureToIndexMap.get(preample + "_" + parameterName);
            if(index!=null) {
                BigDecimal[] values = data.get(timestamp);
                if(values == null) {
                    values = new BigDecimal[46];
                    data.put(timestamp,values);
                }
                BigDecimal value = inner.get("val").getAsBigDecimal();
                values [index] =   value;
            }
        }
    }

    public static void main(String[] args) throws Exception {
        Map<Date, BigDecimal[]> data = new TreeMap<>()  ;
        int year  = 2019 ;
        int month = 11;
        int day = 25;
        Calendar calendar = getCalendar(year, month, day);

        for(int i=0;i<26;i++) {
            Date date = calendar.getTime();
            for (String location : LOCATIONS) {
                String filename = DATA_FILE_HYDRO + location + "_" + dateFormat.format(date);
                getHydroDate(location, new File(filename), data);
            }

            for (String location : LOCATIONS2) {
                String filename = DATA_FILE_SMN + location + "_" + dateFormat.format(date);
                getSmsData(location, new File(filename), data);
            }
            calendar.add(Calendar.DAY_OF_MONTH,1);
        }
        Date from = getCalendar(2019, 11,25).getTime();
        Date to = getCalendar(2019, 12, 20 ).getTime();

        Map<Date, BigDecimal[]> filteredList = new TreeMap<>() ;
        for(Map.Entry<Date, BigDecimal[]> entry : data.entrySet()) {
           if(entry.getKey().getTime()>=from.getTime() && entry.getKey().getTime()<to.getTime()) {
               filteredList.put(entry.getKey(),entry.getValue());
               int missingValues = getMissingValues(entry.getValue());
               if(missingValues>0) {
                   System.out.println(entry.getKey() + " " + missingValues);
               }
           }
        }
        StringBuffer csvContent = new StringBuffer();

        for(int i=0;i<featureToIndexMap.size();i++) {
            for(Map.Entry<String, Integer> entry : featureToIndexMap.entrySet()) {
                if(entry.getValue().equals(i)) {
                    csvContent.append(entry.getKey());
                    if (i != featureToIndexMap.size() - 1) {
                        csvContent.append(",");
                    }
                }
            }
        }
        csvContent.append("\n");
        for(Map.Entry<Date, BigDecimal[]> entry : filteredList.entrySet()) {
            csvContent.append(StringUtils.join(entry.getValue(),",") + "\n");
        }
        System.out.println();
        FileUtils.write(new File("c:/temp/aare/aare_data.csv"),csvContent.toString());
    }

    private static int getMissingValues(BigDecimal[] values) {
        int num = 0;
        for(BigDecimal v : values) {
            if(v==null)  {
                num++;
            }
        }
        return num;

    }

    @NotNull
    private static Calendar getCalendar(int year, int month, int day) {
        Calendar calendar = Calendar.getInstance();
        calendar.setLenient(false);
        calendar.clear();

        calendar.set(year, month-1, day);
        return calendar;
    }

}


