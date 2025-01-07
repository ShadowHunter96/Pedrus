package cz.bbn.cerberus.invoice.dto;

import lombok.extern.slf4j.Slf4j;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

@Slf4j
public class InvoicePdfParser {

    private final String text;

    private List<String> icoList;
    private final List<LocalDate> invoicingDateList = new ArrayList<>();
    private final List<LocalDate> dueDateList = new ArrayList<>();
    private final List<Double> amountList = new ArrayList<>();

    private final List<String> lines = new ArrayList<>();

    public InvoicePdfParser(String text) {
        this.text = text;
        parsePdfString();
    }

    public List<String> getIcoList() {
        return icoList;
    }

    public List<LocalDate> getInvoicingDateList() {
        return invoicingDateList;
    }

    public List<LocalDate> getDueDateList() {
        return dueDateList;
    }

    public List<Double> getAmountList() {
        return amountList;
    }

    private void parsePdfString() {

        lines.addAll(text.lines().toList());

        Pattern datePattern = Pattern.compile("\\d{1,2}(\\/|-|\\.)(\\d{1,2}|[a-zA-Z]{3})(\\/|-|\\.)\\d{2,4}");

        icoList = parseFromText(new String[]{"Dodavatel", "IČ"}, Pattern.compile("\\d{8}"), 0);
        List<String> tempInvoicingDateList = parseFromText(
                new String[]{"Datum vystavení", "Objednávka", "Invoice Date"}, datePattern, 0);
        List<String> tempDueDateList = parseFromText(
                new String[]{"Datum splatnosti", "Objednávka", "Due Date"}, datePattern, 0);
        List<String> tempAmountList = parseFromText(
                new String[]{"K úhradě", "Celkem s DPH", "CELKEM", "Total"},
                Pattern.compile("((\\d{1,3})(.)?)?((\\d{1,3})(.)?)?((\\d{1,3})(.)?)?\\d{1,3}(\\.|,)\\d{2}"), -2);

        for (String invoicingDate : tempInvoicingDateList) {
            LocalDate tempDate = parseDate(invoicingDate);
            if (!invoicingDateList.contains(tempDate)) {
                invoicingDateList.add(tempDate);
            }
        }

        for (String dueDate : tempDueDateList) {
            LocalDate tempDate = parseDate(dueDate);
            if (!dueDateList.contains(tempDate)) {
                dueDateList.add(tempDate);
            }
        }

        for (String amount : tempAmountList) {
            Double tempAmount = parseDouble(amount);
            if (!amountList.contains(tempAmount)) {
                amountList.add(tempAmount);
            }
        }

    }

    private List<String> parseFromText(String[] keyList, Pattern pattern, int candidateLineOffset) {
        List<String> candidateList = new ArrayList<>();
        for (String key : keyList) {
            List<Integer> candidateLineList = new ArrayList<>();

            for (int i = 0; i < lines.size(); i++) {
                if (lines.get(i).toLowerCase().contains(key.toLowerCase())) {
                    candidateLineList.add(i + candidateLineOffset);
                }
            }

            for (Integer candidateLine : candidateLineList) {
                addToList(candidateList, candidateLine, pattern);
            }
        }
        return candidateList;
    }

    private void addToList(List<String> candidateList, Integer candidateLine, Pattern pattern) {
        int lineNumber = 10;

        if (candidateLine != 0 && lines.size() < (candidateLine + lineNumber)) {
            lineNumber = lines.size() - candidateLine;
        }

        if (candidateLine != 0 && lines.size() >= (candidateLine + lineNumber)) {
            for (int i = candidateLine; i < (candidateLine + lineNumber); i++) {
                List<String> processedCandidateList = getTextFromLine(lines.get(i), pattern);
                for (String candidate : processedCandidateList) {
                    if (!"".equals(candidate) && !candidateList.contains(candidate)) {
                        candidateList.add(candidate);
                    }
                }
            }
        }
    }

    private List<String> getTextFromLine(String line, Pattern pattern) {
        Pattern tempPattern = Pattern.compile(",\\d\\d\\s");

        List<Integer> parseInt = new ArrayList<>();
        List<String> parsedLine = new ArrayList<>();

        Matcher tempMatcher = tempPattern.matcher(line);
        String tempStr = line;
        while (tempMatcher.find()) {
            String found = tempMatcher.group();
            parseInt.add(tempStr.indexOf(found) + 4);
            tempStr = tempStr.replaceFirst(found, "XXXX");
        }

        String tempLine = line;
        if (!parseInt.isEmpty()) {
            Collections.reverse(parseInt);
            for (int i : parseInt) {
                parsedLine.add(tempLine.substring(i, tempLine.length() - 1));
                tempLine = tempLine.substring(0, i);
            }
            parsedLine.add(tempLine);
        }

        if (parsedLine.isEmpty()) {
            parsedLine.add(line);
        }

        List<String> textList = new ArrayList<>();
        for (String linePart : parsedLine) {
            String tLine = linePart.replaceAll("\\s", "");
            Matcher matcher = pattern.matcher(tLine);
            while (matcher.find()) {
                textList.add(matcher.group());
            }
        }
        return textList;
    }

    private LocalDate parseDate(String strDate) {
        try {
            String simplifiedDate = "";
            strDate = strDate.strip();
            if (strDate.contains(".")) {
                String[] strArr = strDate.split("\\.");
                simplifiedDate = simplifyDate(strArr);
            }
            if (strDate.contains("/")) {
                String[] strArr = strDate.split("/");
                simplifiedDate = simplifyDate(strArr);
            }

            if (strDate.contains("-")) {
                String[] strArr = strDate.split("-");
                simplifiedDate = simplifyDate(strArr);
            }
            if (simplifiedDate.length() == 10) {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("d.MM.yyyy");
                return LocalDate.parse(simplifiedDate, formatter);
            }
            if (simplifiedDate.length() == 11) {
                DateTimeFormatter formatter = DateTimeFormatter.ofPattern("d.MMM.yyyy", Locale.US);
                return LocalDate.parse(simplifiedDate, formatter);
            }
            if (strDate.length() > 0) {
                return LocalDate.parse(strDate);
            }
        } catch (DateTimeParseException e) {
            log.error("Date parsing error", e);
            return LocalDate.now();
        }
        return LocalDate.now();
    }

    private String simplifyDate(String[] strArr) {
        String day;
        String month;
        String year;
        if (strArr.length == 3) {
            day = strArr[0];
            if (day.length() == 1) {
                day = "0".concat(day);
            }
            month = strArr[1];
            if (month.length() == 1) {
                month = "0".concat(month);
            }
            year = strArr[2];
            if (year.length() == 2) {
                year = "20" + year;
            }
            return day.concat(".").concat(month).concat(".").concat(year);
        }
        return "";
    }

    private Double parseDouble(String strInt) {
        boolean isDouble = strInt.contains("\\.") || strInt.contains(",");
        String value = strInt.replaceAll("[^0-9]", "");

        if (isDouble && value.length() > 1) {
            String beginning = value.substring(0, value.length() - 2);
            String end = value.substring(value.length() - 2);
            value = beginning.concat(".").concat(end);
        }

        if ("".equals(value.strip())) {
            return 0d;
        } else {
            return Double.parseDouble(value);
        }
    }
}
