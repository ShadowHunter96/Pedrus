package cz.bbn.cerberus.holiday.persistance;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.time.LocalDate;
import java.util.Objects;

@Entity
@Table(name = "holidays", schema = "other")
@Getter
@Setter
@NoArgsConstructor
public class HolidayEntity {

    @Id
    private LocalDate date;

    private String description;

    public HolidayEntity(LocalDate date) {
        this.date = date;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        HolidayEntity that = (HolidayEntity) o;
        return date.equals(that.date);
    }

    @Override
    public int hashCode() {
        return Objects.hash(date);
    }
}
