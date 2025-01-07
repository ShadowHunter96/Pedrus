package cz.bbn.cerberus.project.factory;

import cz.bbn.cerberus.contract.factory.ContractFactory;
import cz.bbn.cerberus.contract.persistence.entity.ContractEntity;
import cz.bbn.cerberus.project.dto.ProjectDto;
import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import cz.bbn.cerberus.subject.factory.SubjectFactory;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.user.factory.UserFactory;
import cz.bbn.cerberus.user.persistance.UserEntity;

public class ProjectFactory {

    private ProjectFactory() {
    }

    public static ProjectDto fromEntity(ProjectEntity entity) {
        ProjectDto dto = new ProjectDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setDescription(entity.getDescription());
        dto.setDeleted(entity.getDeleted());
        if (entity.getSubject() != null) {
            dto.setSubject(SubjectFactory.fromEntity(entity.getSubject()));
        }
        dto.setStartTime(entity.getStartTime());
        dto.setEndTime(entity.getEndTime());
        dto.setColor(entity.getColor());
        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        if (entity.getContract() != null) {
            dto.setContract(ContractFactory.fromEntity(entity.getContract()));
        }
        dto.setProjectState(entity.getProjectState());
        return dto;
    }

    public static void fillEntity(ProjectEntity entity, ProjectDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());
        entity.setDescription(dto.getDescription());
        if (dto.getSubject() != null) {
            SubjectEntity subjectEntity = new SubjectEntity();
            SubjectFactory.fillEntity(subjectEntity, dto.getSubject());
            entity.setSubject(subjectEntity);
        }
        entity.setDeleted(false);
        entity.setStartTime(dto.getStartTime());
        entity.setEndTime(dto.getEndTime());
        entity.setColor(dto.getColor());

        UserEntity userEntity = new UserEntity();
        if (dto.getUserDto() != null) {
            UserFactory.fillEntity(userEntity, dto.getUserDto());
        }
        entity.setUserEntity(userEntity);

        if (dto.getContract() != null) {
            ContractEntity contractEntity = new ContractEntity();
            ContractFactory.fillEntity(contractEntity, dto.getContract());
            entity.setContract(contractEntity);
        }
        entity.setProjectState(dto.getProjectState());
    }
}
