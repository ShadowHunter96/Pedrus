package cz.bbn.cerberus.workreport.factory;

import cz.bbn.cerberus.approvement.enums.ApprovementState;
import cz.bbn.cerberus.approvement.factory.ApprovementFactory;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementEntity;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.phase.factory.PhaseFactory;
import cz.bbn.cerberus.phase.repository.PhaseEntity;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.workreport.dto.WorkReportDto;
import cz.bbn.cerberus.workreport.persistance.entity.WorkReportEntity;

import java.util.Optional;

public class WorkReportFactory {

    private WorkReportFactory() {
    }

    public static WorkReportDto fromEntity(WorkReportEntity entity) {
        WorkReportDto dto = new WorkReportDto();
        dto.setId(entity.getId());
        dto.setDate(entity.getDate());
        dto.setItemId(entity.getItemId());
        if (entity.getPhase() != null) {
            dto.setPhaseDto(PhaseFactory.fromEntity(entity.getPhase()));
        }
        if (entity.getActivity() != null) {
            dto.setActivity(EnumerationFactory.fromEntity(entity.getActivity()));
        }
        dto.setDuration(entity.getDuration());
        dto.setDescription(entity.getDescription());
        dto.setEmployeeId(entity.getEmployeeId());
        if(entity.getApprovementEntity() != null) {
            dto.setApprovementDto(ApprovementFactory.fromEntitySimple(entity.getApprovementEntity()));
            String approvementStatus = dto.getApprovementDto().getApprovementState().equals(ApprovementState.COMPLETELY_APPROVED) ?
                    "" : ": ".concat(Transl.get(dto.getApprovementDto().getApprovementState().name()));
            dto.setDescription(Transl.get(dto.getApprovementDto().getApprovementType().name()).concat(approvementStatus));
        }
        return dto;
    }

    public static void fillEntity(WorkReportEntity entity, WorkReportDto dto) {
        entity.setId(dto.getId());
        entity.setDate(dto.getDate());
        if (dto.getPickDto() != null) {
            entity.setItemId(dto.getPickDto().getId());
        }
        if (dto.getPhaseDto() != null) {
            PhaseEntity phaseEntity = new PhaseEntity();
            PhaseFactory.fillEntity(phaseEntity, dto.getPhaseDto());
            entity.setPhase(phaseEntity);
        }
        if (dto.getActivity() != null) {
            EnumerationEntity activityEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(activityEntity, dto.getActivity());
            entity.setActivity(activityEntity);
        }
        entity.setDuration(Optional.ofNullable(dto.getDuration()).orElse(0D));
        entity.setDescription(dto.getDescription());
        entity.setEmployeeId(dto.getEmployeeId());
        if(dto.getApprovementDto() != null) {
            ApprovementEntity approvementEntity = new ApprovementEntity();
            ApprovementFactory.fillEntity(approvementEntity, dto.getApprovementDto());
            entity.setApprovementEntity(approvementEntity);
        }
    }

}
