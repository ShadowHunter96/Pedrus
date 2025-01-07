package cz.bbn.cerberus.workreport.factory;

import cz.bbn.cerberus.workreport.dto.WorkReportClosedDto;
import cz.bbn.cerberus.workreport.persistance.entity.WorkReportClosedEntity;

public class WorkReportClosedFactory {

    private WorkReportClosedFactory() {
    }

    public static WorkReportClosedDto fromEntity(WorkReportClosedEntity entity) {
        WorkReportClosedDto dto = new WorkReportClosedDto();
        dto.setYearMonth(entity.getId().getMonth() + entity.getId().getYear());
        return dto;
    }
}
