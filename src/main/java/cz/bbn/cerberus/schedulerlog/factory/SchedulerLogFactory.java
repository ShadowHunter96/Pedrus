package cz.bbn.cerberus.schedulerlog.factory;

import cz.bbn.cerberus.schedulerlog.dto.SchedulerLogDto;
import cz.bbn.cerberus.schedulerlog.persistance.SchedulerLogEntity;

public class SchedulerLogFactory {

    private SchedulerLogFactory() {
    }

    public static SchedulerLogDto fromEntity(SchedulerLogEntity entity) {
        SchedulerLogDto dto = new SchedulerLogDto();
        dto.setDescription(entity.getDescription());
        dto.setDate(entity.getDate());
        dto.setException(entity.getException());
        return dto;
    }
}
