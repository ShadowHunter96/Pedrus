package cz.bbn.cerberus.project.persistance.repository;

import cz.bbn.cerberus.commons.component.ui.domain.ItemEntity;
import cz.bbn.cerberus.project.persistance.entity.ProjectSimpleEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;

import java.util.List;
import java.util.Set;

public interface ProjectSimpleRepository
        extends JpaRepository<ProjectSimpleEntity, String>, JpaSpecificationExecutor<ProjectSimpleEntity> {

    @Query("select entity.id from ProjectEntity entity where entity.deleted = false")
    List<String> findAllId();

    @Query("select new ItemEntity(entity.id, entity.name) " +
            "from ProjectEntity entity where deleted = false and entity.id in :projectIdSet")
    List<ItemEntity> findAllAllowedItemList(Set<String> projectIdSet);

    @Query(" select new ProjectSimpleEntity(entity.id, entity.name) " +
            "from ProjectEntity entity " +
            "where entity.deleted = false and entity.id in (:projectIdSet)")
    List<ProjectSimpleEntity> findAllAllowedByUser(Set<String> projectIdSet);
}
