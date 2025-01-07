package cz.bbn.cerberus.project.factory;

import cz.bbn.cerberus.project.dto.ProjectSimpleDto;
import cz.bbn.cerberus.project.persistance.entity.ProjectSimpleEntity;
import cz.bbn.cerberus.user.factory.UserFactory;

public class ProjectSimpleFactory {

    private ProjectSimpleFactory() {
    }

    public static ProjectSimpleDto fromEntity(ProjectSimpleEntity entity) {
        ProjectSimpleDto dto = new ProjectSimpleDto();
        dto.setId(entity.getId());
        dto.setName(entity.getName());
        dto.setSubject(entity.getSubjectId());
        dto.setDeleted(entity.getDeleted());
        if (entity.getUserEntity() != null) {
            dto.setUserDto(UserFactory.fromEntity(entity.getUserEntity()));
        }
        return dto;
    }

    public static void fillEntity(ProjectSimpleEntity entity, ProjectSimpleDto dto) {
        entity.setId(dto.getId());
        entity.setName(dto.getName());

        entity.setSubjectId(dto.getSubject());
        entity.setDeleted(false);
    }
}
