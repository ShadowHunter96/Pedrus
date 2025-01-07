package cz.bbn.cerberus.workreport.dto;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;
import java.time.Month;
import java.time.format.TextStyle;
import java.util.Locale;
import java.util.Objects;

@Getter
@Setter
@AllArgsConstructor
public class YearMonthDto {

    private String month;
    private String year;

    private LocalDate date;

    public YearMonthDto(int month, String year) {
        this.month = Month.of(month).getDisplayName(TextStyle.FULL, Locale.ENGLISH);
        this.year = year;
        this.date = LocalDate.now().withMonth(month).withYear(Integer.parseInt(year));
    }


    public String getYearAndMonth(){
        return date.getMonth().getValue() + "," + date.getYear();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        YearMonthDto that = (YearMonthDto) o;
        return Objects.equals(month, that.month) && Objects.equals(year, that.year);
    }

    @Override
    public int hashCode() {
        return Objects.hash(month, year);
    }
}
