package cz.bbn.cerberus.marekdemo.repository;


import cz.bbn.cerberus.marekdemo.entity.MarekEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.List;

/**
 * Created by marek.vu on 04.10.2023.
 */

@Repository
public interface MarekRepository extends JpaRepository<MarekEntity, Long>, JpaSpecificationExecutor<MarekEntity> {
    @Query("select m from MarekEntity m " +
            "where lower(m.name) like lower(concat('%', :searchTerm, '%')) " +
            "or lower(m.description) like lower(concat('%', :searchTerm, '%'))")
    List<MarekEntity> search(@Param("searchTerm") String searchTerm);
}
