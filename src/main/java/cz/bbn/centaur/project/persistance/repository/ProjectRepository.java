package cz.bbn.cerberus.project.persistance.repository;

import cz.bbn.cerberus.project.persistance.entity.ProjectEntity;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

public interface ProjectRepository extends JpaRepository<ProjectEntity, String>,
        JpaSpecificationExecutor<ProjectEntity> {

    @Query("select entity.id from ProjectEntity entity order by entity.id desc")
    String findLastId(PageRequest pageRequest);

    @Modifying
    @Query("update ProjectEntity entity set entity.userEntity.id = :userId where entity.id = :id")
    void updateOwner(Long userId, String id);

    @Modifying
    @Query("update ProjectEntity entity set entity.userEntity.id = :newOwnerId where entity.userEntity.id = :ownerId")
    void updateOwner(Long ownerId, Long newOwnerId);

}
