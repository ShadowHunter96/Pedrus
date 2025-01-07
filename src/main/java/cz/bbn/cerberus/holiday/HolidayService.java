package cz.bbn.cerberus.holiday;

import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.holiday.persistance.HolidayRepository;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class HolidayService {

    private final HolidayRepository holidayRepository;

    public HolidayService(HolidayRepository holidayRepository) {
        this.holidayRepository = holidayRepository;
    }

    public List<HolidayEntity> findAll() {
        return this.holidayRepository.findAll();
    }
}
