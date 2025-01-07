package cz.bbn.cerberus.holiday.persistance;

import org.springframework.data.jpa.repository.JpaRepository;

import java.time.LocalDate;

public interface HolidayRepository extends JpaRepository<HolidayEntity, LocalDate> {
}
