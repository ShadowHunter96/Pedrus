package cz.bbn.cerberus.approvement.factory;

import cz.bbn.cerberus.approvement.dto.ApprovementBusinessTripDto;
import cz.bbn.cerberus.approvement.dto.ApprovementDto;
import cz.bbn.cerberus.approvement.dto.ApprovementProjectEmployeeDto;
import cz.bbn.cerberus.approvement.dto.ApprovementSimpleDto;
import cz.bbn.cerberus.approvement.enums.ApprovementType;
import cz.bbn.cerberus.approvement.enums.BusinessTripTransportationType;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementBusinessTripEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementProjectEmployeeEntity;
import cz.bbn.cerberus.approvement.persistance.entity.ApprovementProjectEmployeeId;
import cz.bbn.cerberus.employee.dto.EmployeeDto;
import cz.bbn.cerberus.employee.factory.EmployeeFactory;
import cz.bbn.cerberus.employee.persistance.entity.EmployeeEntity;
import cz.bbn.cerberus.enumeration.factory.EnumerationFactory;
import cz.bbn.cerberus.enumeration.persistance.entity.EnumerationEntity;
import cz.bbn.cerberus.opportunity.factory.OpportunityFactory;
import cz.bbn.cerberus.opportunity.persistance.entity.OpportunityEntity;
import cz.bbn.cerberus.project.factory.ProjectFactory;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import cz.bbn.cerberus.role.factory.RoleFactory;
import cz.bbn.cerberus.role.persistance.entity.RoleEntity;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

public class ApprovementFactory {

    private ApprovementFactory() {
    }

    public static ApprovementSimpleDto fromSimpleEntity(ApprovementEntity entity) {
        ApprovementSimpleDto dto = new ApprovementSimpleDto();
        dto.setId(entity.getId());
        dto.setDateFrom(entity.getDateFrom());
        dto.setDateTo(entity.getDateTo());
        dto.setApprovementState(entity.getApprovementState());
        dto.setApprovementType(entity.getApprovementType());
        dto.setLineManageApproved(entity.getLineManageApproved());
        dto.setSuperiorApproved(entity.getSuperiorApproved());
        dto.setCreatedUserDto(UserFactory.fromEntity(entity.getCreatedUserEntity()));
        dto.setOwnerUserDto(UserFactory.fromEntity(entity.getOwnerUserEntity()));
        dto.setCreated(entity.getCreated());
        dto.setDays(entity.getDays());
        if (entity.getApprovementBusinessTripEntity() != null) {
            dto.setApprovementBusinessTripDto(
                    ApprovementFactory.fromApprovementBusinessTripEntity(entity.getApprovementBusinessTripEntity()));
        }
        if (entity.getLineManagerUserEntity() != null) {
            dto.setLineManagerId(entity.getLineManagerUserEntity().getId());
        }
        if (entity.getLineManagerRoleEntity() != null) {
            dto.setLineManagerRoleId(entity.getLineManagerRoleEntity().getId());
        }
        if (entity.getSuperiorUserEntity() != null) {
            dto.setSuperiorId(entity.getSuperiorUserEntity().getId());
        }
        if (entity.getSuperiorRoleEntity() != null) {
            dto.setSuperiorRoleId(entity.getSuperiorRoleEntity().getId());
        }
        return dto;
    }

    public static ApprovementDto fromEntitySimple(ApprovementEntity entity) {
        ApprovementDto dto = new ApprovementDto();
        dto.setId(entity.getId());
        dto.setDateFrom(entity.getDateFrom());
        dto.setDateTo(entity.getDateTo());
        dto.setApprovementState(entity.getApprovementState());
        dto.setApprovementType(entity.getApprovementType());
        return dto;
    }

    public static ApprovementDto fromEntity(ApprovementEntity entity) {
        ApprovementDto dto = new ApprovementDto();
        dto.setId(entity.getId());
        dto.setDateFrom(entity.getDateFrom());
        dto.setDateTo(entity.getDateTo());
        dto.setApprovementState(entity.getApprovementState());
        dto.setApprovementType(entity.getApprovementType());
        if (entity.getApprovementProjectEmployeeEntity() != null
                && !entity.getApprovementProjectEmployeeEntity().isEmpty()) {
            List<ApprovementProjectEmployeeDto> approvementProjectEmployeeDtoList = new ArrayList<>();
            entity.getApprovementProjectEmployeeEntity().forEach(approvementProjectEmployeeEntity ->
                    approvementProjectEmployeeDtoList.add(
                            fromApprovementProjectEmployeeEntity(approvementProjectEmployeeEntity)));
            dto.setApprovementProjectEmployeeDtoList(approvementProjectEmployeeDtoList);
            dto.setFirstProjectDto(approvementProjectEmployeeDtoList.get(0).getProjectDto());
        } else if (entity.getApprovementType().equals(ApprovementType.ILL)) {
            dto.setFirstEmployeeDto(EmployeeFactory.fromEntity(entity.getOwnerUserEntity().getEmployee()));
        } else {
            dto.setApprovementProjectEmployeeDtoList(new ArrayList<>());
        }
        dto.setCreated(entity.getCreated());
        dto.setNote(entity.getNote());
        dto.setHalfDay(entity.getHalfDay());
        dto.setOwnerUserDto(UserFactory.fromEntity(entity.getOwnerUserEntity()));
        dto.setCreatedUserDto(UserFactory.fromEntity(entity.getCreatedUserEntity()));

        if (entity.getLineManagerUserEntity() != null) {
            dto.setLineManagerUserDto(UserFactory.fromEntity(entity.getLineManagerUserEntity(), false));
        }
        dto.setLineManageNote(entity.getLineManageNote());
        dto.setLineManageApproved(entity.getLineManageApproved());
        dto.setLineManageApprovedDate(entity.getLineManageApprovedDate());

        if (entity.getSuperiorUserEntity() != null) {
            dto.setSuperiorUserDto(UserFactory.fromEntity(entity.getSuperiorUserEntity(), false));
        }
        dto.setSuperiorNote(entity.getSuperiorNote());
        dto.setSuperiorApprovedDate(entity.getSuperiorApprovedDate());
        dto.setSuperiorApproved(entity.getSuperiorApproved());

        dto.setLMSuperiorEquals(dto.getLineManagerUserDto() != null
                && dto.getLineManagerUserDto().getId().equals(dto.getSuperiorUserDto().getId()));

        dto.setDays(entity.getDays());
        if (entity.getLineManagerRoleEntity() != null) {
            dto.setLineManagerRole(RoleFactory.fromEntity(entity.getLineManagerRoleEntity()));
        }
        if (entity.getApprovementBusinessTripEntity() != null) {
            dto.setApprovementBusinessTripDto(
                    fromApprovementBusinessTripEntity(entity.getApprovementBusinessTripEntity()));
        }
        if (entity.getEnumeration() != null) {
            dto.setEnumeration(EnumerationFactory.fromEntity(entity.getEnumeration()));
        }

        dto.setCreatedForEmployeeDto(EmployeeFactory.fromEntity(entity.getOwnerUserEntity().getEmployee()));
        return dto;
    }

    public static ApprovementProjectEmployeeDto fromApprovementProjectEmployeeEntity(
            ApprovementProjectEmployeeEntity entity) {
        ApprovementProjectEmployeeDto dto = new ApprovementProjectEmployeeDto();
        dto.setEmployeeDto(EmployeeFactory.fromEntity(entity.getId().getEmployeeEntity()));
        dto.setProjectDto(ProjectFactory.fromEntity(entity.getId().getProjectEntity()));
        dto.setApprovementId(entity.getId().getApprovementId());
        return dto;
    }

    public static ApprovementBusinessTripDto fromApprovementBusinessTripEntity(ApprovementBusinessTripEntity entity) {
        ApprovementBusinessTripDto dto = new ApprovementBusinessTripDto();
        dto.setId(entity.getId());
        if (entity.getApprovementFromSubjectEntity() != null) {
            dto.setApprovementFromSubjectDto(SubjectFactory.fromEntity(entity.getApprovementFromSubjectEntity()));
        }
        if (entity.getApprovementToSubjectEntity() != null) {
            dto.setApprovementToSubjectDto(SubjectFactory.fromEntity(entity.getApprovementToSubjectEntity()));
        }
        dto.setApprovementToAnother(entity.getApprovementToAnother());
        if (entity.getPurposeEntity() != null) {
            dto.setPurposeDto(EnumerationFactory.fromEntity(entity.getPurposeEntity()));
        }

        if (entity.getBusinessTripTransportationType() != null) {
            String[] transportations = entity.getBusinessTripTransportationType().split(",");
            Set<BusinessTripTransportationType> businessTripTransportationTypeSet = new HashSet<>();
            Arrays.stream(transportations).forEach(s ->
                    businessTripTransportationTypeSet.add(BusinessTripTransportationType.valueOf(s))
            );
            dto.setBusinessTripTransportationTypeSet(businessTripTransportationTypeSet);
        }

        dto.setInterruptionFrom(entity.getInterruptionFrom());
        dto.setInterruptionTo(entity.getInterruptionTo());
        if (entity.getProjectEntity() != null) {
            dto.setProjectDto(ProjectFactory.fromEntity(entity.getProjectEntity()));
        }
        if (entity.getOpportunityEntity() != null) {
            dto.setOpportunityDto(OpportunityFactory.fromEntity(entity.getOpportunityEntity()));
        }
        if (entity.getFellowPassengers() != null) {
            String[] fellowPassengers = entity.getFellowPassengers().split(",");
            Set<EmployeeDto> fellowPassengerSet = new HashSet<>();
            Arrays.stream(fellowPassengers).forEach(s -> {
                        EmployeeDto employeeDto = new EmployeeDto();
                        employeeDto.setId(s);
                        fellowPassengerSet.add(employeeDto);
                    }
            );
            dto.setFellowPassengers(fellowPassengerSet);
        }
        return dto;
    }

    public static void fillEntity(ApprovementBusinessTripEntity entity, ApprovementBusinessTripDto dto) {
        entity.setId(dto.getId());
        if (dto.getApprovementFromSubjectDto() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getApprovementFromSubjectDto());
            entity.setApprovementFromSubjectEntity(subjectEntity);
        }
        if (dto.getApprovementToSubjectDto() != null && dto.getApprovementToSubjectDto().getId() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getApprovementToSubjectDto());
            entity.setApprovementToSubjectEntity(subjectEntity);
        }
        entity.setApprovementToAnother(dto.getApprovementToAnother());

        if (dto.getPurposeDto() != null) {
            EnumerationEntity purposeEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(purposeEntity, dto.getPurposeDto());
            entity.setPurposeEntity(purposeEntity);
        }

        if (!dto.getBusinessTripTransportationTypeSet().isEmpty()) {
            String transportation = StringUtils.join(dto.getBusinessTripTransportationTypeSet(), ",");
            entity.setBusinessTripTransportationType(transportation);
        }
        entity.setInterruptionFrom(dto.getInterruptionFrom());
        entity.setInterruptionTo(dto.getInterruptionTo());

        if (dto.getProjectDto() != null) {
            ProjectEntity projectEntity = new ProjectEntity();
            ProjectFactory.fillEntity(projectEntity, dto.getProjectDto());
            entity.setProjectEntity(projectEntity);
        }

        if (dto.getOpportunityDto() != null) {
            OpportunityEntity opportunityEntity = new OpportunityEntity();
            OpportunityFactory.fillEntity(opportunityEntity, dto.getOpportunityDto());
            entity.setOpportunityEntity(opportunityEntity);
        }

        if (dto.getFellowPassengers() != null) {
            AtomicReference<String> fellowPassengers = new AtomicReference<>("");
            dto.getFellowPassengers().forEach(employeeDto ->
                    fellowPassengers.set(fellowPassengers.get() + employeeDto.getId() + ","));
            String value = fellowPassengers.get();
            entity.setFellowPassengers(value.substring(0, value.length() - 1));
        }

    }

    public static void fillEntity(Long id, ApprovementProjectEmployeeEntity entity, ApprovementProjectEmployeeDto dto) {
        ApprovementProjectEmployeeId appId = new ApprovementProjectEmployeeId();
        appId.setApprovementId(id);
        ProjectEntity projectEntity = new ProjectEntity();
        projectEntity.setId(dto.getProjectDto().getId());
        appId.setProjectEntity(projectEntity);
        EmployeeEntity employeeEntity = new EmployeeEntity();
        employeeEntity.setId(dto.getEmployeeDto().getId());
        appId.setEmployeeEntity(employeeEntity);
        entity.setId(appId);
    }

    public static void fillEntity(ApprovementEntity entity, ApprovementDto dto) {
        entity.setId(dto.getId());
        entity.setDateFrom(dto.getDateFrom());
        entity.setDateTo(dto.getDateTo());

        UserEntity userEntity = new UserEntity();
        UserFactory.fillEntity(userEntity, dto.getOwnerUserDto());
        entity.setOwnerUserEntity(userEntity);

        if (dto.getCreatedUserDto() != null) {
            UserEntity createdUserEntity = new UserEntity();
            UserFactory.fillEntity(createdUserEntity, dto.getCreatedUserDto());
            entity.setCreatedUserEntity(createdUserEntity);
        }

        entity.setApprovementType(dto.getApprovementType());
        entity.setApprovementState(dto.getApprovementState());
        entity.setCreated(dto.getCreated());
        entity.setNote(dto.getNote());

        List<ApprovementProjectEmployeeEntity> approvementProjectEmployeeEntityList = new ArrayList<>();
        if (dto.getApprovementProjectEmployeeDtoList() != null) {
            dto.getApprovementProjectEmployeeDtoList().forEach(actualDto -> {
                ApprovementProjectEmployeeEntity approvementProjectEmployeeEntity = new ApprovementProjectEmployeeEntity();
                fillEntity(dto.getId(), approvementProjectEmployeeEntity, actualDto);
                approvementProjectEmployeeEntityList.add(approvementProjectEmployeeEntity);
            });
        }
        entity.setApprovementProjectEmployeeEntity(approvementProjectEmployeeEntityList);

        if (dto.getLineManagerUserDto() != null) {
            UserEntity lineManagerEntity = new UserEntity();
            UserFactory.fillEntity(lineManagerEntity, dto.getLineManagerUserDto());
            entity.setLineManagerUserEntity(lineManagerEntity);
        }

        entity.setLineManageApproved(dto.getLineManageApproved());
        entity.setLineManageNote(dto.getLineManageNote());
        entity.setLineManageApprovedDate(dto.getLineManageApprovedDate());

        if (dto.getSuperiorUserDto() != null) {
            UserEntity superiorEntity = new UserEntity();
            UserFactory.fillEntity(superiorEntity, dto.getSuperiorUserDto());
            entity.setSuperiorUserEntity(superiorEntity);
        }

        entity.setSuperiorApproved(dto.getSuperiorApproved());
        entity.setSuperiorNote(dto.getSuperiorNote());
        entity.setSuperiorApprovedDate(dto.getSuperiorApprovedDate());

        entity.setHalfDay(dto.getHalfDay());
        entity.setDays(dto.getDays());

        if (dto.getLineManagerRole() != null) {
            RoleEntity roleEntity = new RoleEntity();
            RoleFactory.fillEntity(roleEntity, dto.getLineManagerRole());
            entity.setLineManagerRoleEntity(roleEntity);
        }

        if (dto.getEnumeration() != null) {
            EnumerationEntity enumEntity = new EnumerationEntity();
            EnumerationFactory.fillEntity(enumEntity, dto.getEnumeration());
            entity.setEnumeration(enumEntity);
        }
    }
}
