package cz.bbn.cerberus.tasktemplate.persistance.repository;

import cz.bbn.cerberus.tasktemplate.persistance.entity.TaskTemplateEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Set;

public interface TaskTemplateRepository extends JpaRepository<TaskTemplateEntity, Long>,
        JpaSpecificationExecutor<TaskTemplateEntity> {

    @Query("select entity from TaskTemplateEntity entity " +
            "where entity.allowedRole.id in :activeRoleSet and entity.deleted != true")
    List<TaskTemplateEntity> findAllowedTemplateList(Set<String> activeRoleSet);
}
