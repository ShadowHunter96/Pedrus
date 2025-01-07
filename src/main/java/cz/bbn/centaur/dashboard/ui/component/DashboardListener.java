package cz.bbn.cerberus.dashboard.ui.component;

import cz.bbn.cerberus.dashboard.dto.CalendarMonthDto;

import java.time.LocalDate;

public interface DashboardListener {

    void openWorkReport(LocalDate day);

    CalendarMonthDto getMonthData(LocalDate date);
}
