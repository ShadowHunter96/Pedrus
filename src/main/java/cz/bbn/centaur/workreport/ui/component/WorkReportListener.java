package cz.bbn.cerberus.workreport.ui.component;

import cz.bbn.cerberus.commons.component.ui.interfaces.ConfirmAction;
import cz.bbn.cerberus.phase.dto.PhaseDto;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.dto.WorkReportPickDto;
import cz.bbn.cerberus.workreport.dto.YearMonthDto;

public interface WorkReportListener {

    void changeYearMonthValue(YearMonthDto yearMonthDto);

    void changeProjectAndPhase(WorkReportPickDto project, PhaseDto phase);

    ConfirmAction getConfirmAction(WorkReportDto workReportDto);

    void openEditDialog(WorkReportDto dto, boolean enableActions);
}
