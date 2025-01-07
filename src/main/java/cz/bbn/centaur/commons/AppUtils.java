package cz.bbn.cerberus.commons;

import com.vaadin.flow.component.UI;
import com.vaadin.flow.component.button.Button;
import com.vaadin.flow.component.datepicker.DatePicker;
import cz.bbn.cerberus.commons.component.ui.breadcrump.HistoryBreadcrumbs;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.component.ui.interfaces.FilterAction;
import cz.bbn.cerberus.commons.enums.QuarterDateFilter;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.phoneprefix.dto.PhonePrefixDto;
import cz.bbn.cerberus.translation.enums.ApplicationTranslation;
import cz.bbn.cerberus.user.UserService;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;
import org.apache.commons.lang3.Range;
import org.apache.commons.lang3.StringUtils;
import org.springframework.web.util.UriUtils;

import javax.persistence.criteria.CriteriaBuilder;
import javax.persistence.criteria.Path;
import javax.persistence.criteria.Predicate;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.DayOfWeek;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.format.DateTimeFormatter;
import java.time.format.TextStyle;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

public class AppUtils {

    public static final String DATE_FORMAT_CZ = "dd.MM.yyyy";
    public static final String DATE_FORMAT_EN = "dd/MM/yyyy";
    public static final String DAY_MONTH_FORMAT = "dd.MM.";
    public static final String DAY_FORMAT = "d";

    private AppUtils() {
    }

    public static String formatDateTime(LocalDateTime localDateTime, boolean showTime) {
        if (localDateTime == null) {
            return "";
        }
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DATE_FORMAT_CZ.concat(showTime ? " HH:mm" : ""));

        if (UserService.getApplicationTranslation() == ApplicationTranslation.EN) {
            formatter = DateTimeFormatter.ofPattern(DATE_FORMAT_EN.concat(showTime ? " HH:mm" : ""));
        }

        return formatter.format(localDateTime);
    }

    public static String formatDate(LocalDate localDate) {
        if (localDate == null) {
            return "";
        }
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(DATE_FORMAT_CZ);

        if (UserService.getApplicationTranslation() == ApplicationTranslation.EN) {
            formatter = DateTimeFormatter.ofPattern(DATE_FORMAT_EN);
        }

        return formatter.format(localDate);
    }

    public static String formatDayMonth(LocalDate localDate, String format) {
        if (localDate == null) {
            return "";
        }

        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(format);
        return formatter.format(localDate);
    }

    public static String formatDayMonth(LocalDate localDate) {
        return formatDayMonth(localDate, DAY_MONTH_FORMAT);
    }


    public static String priceWithDecimal(BigDecimal price) {
        if (price == null) {
            return "";
        }
        return priceWithDecimal(price.doubleValue());
    }

    public static String priceWithDecimal(Double price) {
        if (price == null) {
            return "";
        }
        DecimalFormat formatter = new DecimalFormat("###,###,##0.00",
                DecimalFormatSymbols.getInstance(Locale.forLanguageTag("cs")));
        return formatter.format(price);
    }

    public static String priceInteger(Integer price) {
        if (price == null) {
            return "";
        }
        DecimalFormat formatter = new DecimalFormat("###,###,###",
                DecimalFormatSymbols.getInstance(Locale.forLanguageTag("cs")));
        return formatter.format(price);
    }

    public static void addRfClassToGridButton(Button button, String appId) {
        button.addClassName(appId.concat("-rf").replaceAll("\\s+", ""));
    }

    public static Predicate stripAccentLike(CriteriaBuilder criteriaBuilder, Path<String> path, String variable) {
        return criteriaBuilder.like(criteriaBuilder.lower(
                        criteriaBuilder.function("unaccent", String.class, path)),
                "%" + StringUtils.stripAccents(variable.toLowerCase() + "%"));
    }

    public static LocalDate getMonthStart(LocalDate localDate) {
        return YearMonth.from(localDate).atDay(1);
    }

    public static LocalDate getMonthEnd(LocalDate localDate) {
        return YearMonth.from(localDate).atEndOfMonth();
    }

    public static LocalDateTime getMonthStartWithTime(LocalDate localDate) {
        return YearMonth.from(localDate).atDay(1).atStartOfDay();
    }

    public static LocalDateTime getMonthEndWithTime(LocalDate localDate) {
        return YearMonth.from(localDate).atEndOfMonth().atTime(23, 59, 59);
    }

    public static String phoneFormat(PhonePrefixDto phonePrefixDto, String phone) {
        if (phone != null && StringUtils.containsNone(phone, " ")) {
            phone = phone.replaceAll("...", "$0 ");
        }
        return phonePrefixDto != null ? phonePrefixDto.getPhonePrefix().concat(" ").concat(phone) : phone;
    }

    public static Map<String, String> getMapByParams(String params) {
        if (params == null) {
            return new HashMap<>();
        }
        params = UriUtils.decode(params, "UTF-8");
        String[] paramsArray = params.split("&");
        Map<String, String> map = new HashMap<>();
        Arrays.stream(paramsArray).forEach(string -> {
            if (!string.isEmpty() && string.length() > 1) {
                String[] param = string.split("=");
                map.put(param[0], param[1]);
            }
        });
        return map;
    }

    public static String getUrlFromMap(Map<String, String> map) {
        AtomicReference<String> params = new AtomicReference<>("");
        map.forEach((s, s2) -> params.set(params.get() + s + "=" + s2 + "&"));
        return params.get();
    }

    public static String fillParam(FilterAction filterAction, String route, HistoryBreadcrumbs historyBreadcrumbs,
                                   Map<String, String> map, String key, String value) {
        if (value == null && map.containsKey(key)) {
            map.remove(key);
        } else if (map.containsKey(key)) {
            map.replace(key, value);
        } else {
            map.put(key, value);
        }
        filterAction.filter();
        String paramUrl = route + "/" + AppUtils.getUrlFromMap(map);
        UI.getCurrent().getPage().getHistory().pushState(null, paramUrl);
        historyBreadcrumbs.refreshLastBreadcrumb(paramUrl);
        return paramUrl;
    }


    public static String fillParam(FilterAction filterAction, String route, HistoryBreadcrumbs historyBreadcrumbs,
                                   Map<String, String> map, String key, LocalDate value) {
        String stringValue = value == null ? null : value.toString();
        return fillParam(filterAction, route, historyBreadcrumbs, map, key, stringValue);
    }

    public static LocalDate getQuarterFrom(LocalDate from) {
        Integer month = from.getMonth().getValue();
        if (Range.between(1, 3).contains(month)) {
            return LocalDate.now().withMonth(1).withDayOfMonth(1);
        } else if (Range.between(4, 6).contains(month)) {
            return LocalDate.now().withMonth(4).withDayOfMonth(1);
        } else if (Range.between(7, 9).contains(month)) {
            return LocalDate.now().withMonth(7).withDayOfMonth(1);
        } else if (Range.between(10, 12).contains(month)) {
            return LocalDate.now().withMonth(10).withDayOfMonth(1);
        }
        return LocalDate.now();
    }

    public static LocalDate getQuarterTo(LocalDate to) {
        Integer month = to.getMonth().getValue();
        if (Range.between(1, 3).contains(month)) {
            return LocalDate.now().withMonth(3).withDayOfMonth(YearMonth.of(LocalDate.now().getYear(), 3).lengthOfMonth());
        } else if (Range.between(4, 6).contains(month)) {
            return LocalDate.now().withMonth(6).withDayOfMonth(YearMonth.of(LocalDate.now().getYear(), 6).lengthOfMonth());
        } else if (Range.between(7, 9).contains(month)) {
            return LocalDate.now().withMonth(9).withDayOfMonth(YearMonth.of(LocalDate.now().getYear(), 9).lengthOfMonth());
        } else if (Range.between(10, 12).contains(month)) {
            return LocalDate.now().withMonth(12).withDayOfMonth(YearMonth.of(LocalDate.now().getYear(), 12).lengthOfMonth());
        }
        return LocalDate.now();
    }

    public static QuarterDateFilter getQuarterDateFilterValue(LocalDate from) {
        Integer month = from.getMonth().getValue();
        if (Range.between(1, 3).contains(month)) {
            return QuarterDateFilter.FIRST_QUARTER;
        } else if (Range.between(4, 6).contains(month)) {
            return QuarterDateFilter.SECOND_QUARTER;
        } else if (Range.between(7, 9).contains(month)) {
            return QuarterDateFilter.THIRD_QUARTER;
        } else if (Range.between(10, 12).contains(month)) {
            return QuarterDateFilter.FOURTH_QUARTER;
        }
        return QuarterDateFilter.ACTUAL_MONTH;
    }

    public static void setDatesByQuarterFilter(QuarterDateFilter filter, DatePicker from, DatePicker to) {
        if (from == null || to == null) {
            return;
        }
        switch (filter) {
            case ACTUAL_MONTH -> {
                LocalDate localDate = LocalDate.now();
                from.setValue(localDate.withDayOfMonth(1));
                to.setValue(localDate.withDayOfMonth(YearMonth.of(localDate.getYear(), localDate.getMonth()).lengthOfMonth()));
            }
            case FIRST_QUARTER -> {
                from.setValue(getQuarterFrom(LocalDate.now().withMonth(1)));
                to.setValue(getQuarterTo(LocalDate.now().withMonth(3)));
            }
            case SECOND_QUARTER -> {
                from.setValue(getQuarterFrom(LocalDate.now().withMonth(4)));
                to.setValue(getQuarterTo(LocalDate.now().withMonth(6)));
            }
            case THIRD_QUARTER -> {
                from.setValue(getQuarterFrom(LocalDate.now().withMonth(7)));
                to.setValue(getQuarterTo(LocalDate.now().withMonth(9)));
            }
            case FOURTH_QUARTER -> {
                from.setValue(getQuarterFrom(LocalDate.now().withMonth(10)));
                to.setValue(getQuarterTo(LocalDate.now().withMonth(12)));
            }
        }
    }

    public static Map<ItemDto, Double> removeZeroAndGetMap(Map<ItemDto, Double> actualMap) {
        Map<ItemDto, Double> map = new HashMap<>();
        actualMap.forEach((itemDto, value) -> {
            if (value != 0) {
                map.put(itemDto, value);
            }
        });
        return map;
    }

    public static Map<ItemDto, Double> getUserMapWithDouble(List<UserDto> userDtoList) {
        Map<ItemDto, Double> actualMap = new HashMap<>();
        userDtoList.forEach(userDto -> actualMap.put(new ItemDto(userDto), 0D));
        return actualMap;
    }

    public static String generateUrl(String projectUrl, String route, String id, String name) {
        return "<a href='"
                .concat(projectUrl)
                .concat("/")
                .concat(route)
                .concat("/")
                .concat(id)
                .concat("'>")
                .concat(name)
                .concat("</a>");
    }

    public static List<Integer> getYears(int yearsBack) {
        List<Integer> yearList = new ArrayList<>();
        LocalDate date = LocalDate.now();
        yearList.add(date.plusYears(1).getYear());
        for (int i = 0; i < yearsBack; i++) {
            yearList.add(date.getYear());
            date = date.minusYears(1);
        }
        return yearList;
    }

    public static double countWorkDays(LocalDate dateFrom, LocalDate dateTo, List<HolidayEntity> holidayEntityList) {
        int days = 0;
        if (dateFrom != null && dateTo != null && (dateFrom.isBefore(dateTo) || dateFrom.isEqual(dateTo))) {
            int allDays = (int) ChronoUnit.DAYS.between(dateFrom, dateTo) + 1;
            for (int i = 0; i < allDays; i++) {
                if (!dateFrom.getDayOfWeek().equals(DayOfWeek.SATURDAY) && !dateFrom.getDayOfWeek().equals(DayOfWeek.SUNDAY)
                        && !holidayEntityList.contains(new HolidayEntity(dateFrom))) {
                    days++;
                }
                dateFrom = dateFrom.plusDays(1);
            }
        }
        return days;
    }

    public static String cutText(String text, int count, boolean addDots) {
        String actualText = StringUtils.trimToEmpty(text);
        boolean isTextCut = false;
        if (actualText.length() > count) {
            isTextCut = true;
            actualText = text.substring(0, count);
        }
        return addDots && isTextCut ? actualText.concat("...") : actualText;
    }

    public static List<YearMonthDto> getYearMonthList() {

        List<YearMonthDto> yearMonthList = new ArrayList<>();
        LocalDate now = LocalDate.now();
        yearMonthList.add(new YearMonthDto(now.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH),
                String.valueOf(now.getYear()), now));
        for (int i = 1; i < 121; i++) {
            LocalDate then = now.minusMonths(i);
            yearMonthList.add(new YearMonthDto(then.getMonth().getDisplayName(TextStyle.FULL, Locale.ENGLISH),
                    String.valueOf(then.getYear()), then));
        }
        return yearMonthList;
    }

    public static String doubleToString(Double value){
        if(value == null){
            return "0";
        }
        return (value % 1) == 0 ? String.valueOf(value.intValue()) : String.valueOf(value);
    }

    public static int getSequenceFromId(String id) {
        if (id != null) {
            String[] idArr = id.split("-");
            if (idArr.length > 2) {
                return Integer.parseInt(idArr[2]);
            }
        }
        return -1;
    }

    public static String generateId(String type, int sequence) {
        return generateIdCore(type, sequence);
    }

    public static String generateIdSuffix(String type, int sequence, String suffix) {
        return generateIdCore(type, sequence).concat("-").concat(suffix);
    }

    public static String generateIdAddition(String oldId, int subsequence) {
        return oldId.concat("-").concat(String.valueOf(subsequence));
    }

    private static String generateIdCore(String type, int sequence) {
        String id = LocalDate.now().format(DateTimeFormatter.ofPattern("yyMMdd")).concat("-").concat(type).concat("-");
        String sequenceString = "" + sequence;
        StringBuilder toReturn = new StringBuilder();
        if (sequenceString.length() < 5) {
            toReturn.append("0".repeat(4 - sequenceString.length()));
        }
        return id.concat(toReturn.toString()).concat(sequenceString);
    }

}
