package cz.bbn.cerberus.workreport.ui.component;

import com.vaadin.flow.data.binder.Binder;
import cz.bbn.cerberus.commons.component.ui.dialog.AppDialog;
import cz.bbn.cerberus.holiday.persistance.HolidayEntity;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;

import java.util.List;

public interface WorkReportSaveListener {

    void save(Binder<WorkReportDto> binder, AppDialog dialog, WorkReportDto originalDto,
              boolean showDialog, List<HolidayEntity> holidayEntityList);
}
