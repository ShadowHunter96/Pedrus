package cz.bbn.cerberus.suppliertype.persistance;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import java.util.List;

public interface SupplierTypeRepository extends JpaRepository<SupplierTypeEntity, String> {

    @Query(" select entity " +
            "from SupplierTypeEntity entity " +
            "where entity.allowed = true and entity.id != :exceptId")
    List<SupplierTypeEntity> findAllAllowedExceptOne(String exceptId);

    @Query("select entity.allowed " +
            "from SupplierTypeEntity entity " +
            "where entity.id = :id")
    boolean isContactPersonTypeAllowed(String id);

    @Query("select entity from SupplierTypeEntity entity where entity.allowed = true")
    List<SupplierTypeEntity> findAllAllowed();
}
