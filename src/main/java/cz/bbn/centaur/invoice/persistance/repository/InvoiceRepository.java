package cz.bbn.cerberus.invoice.persistance.repository;

import cz.bbn.cerberus.invoice.persistance.entity.InvoiceEntity;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.JpaSpecificationExecutor;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;

import java.time.LocalDate;
import java.util.List;
import java.util.Set;

public interface InvoiceRepository extends JpaRepository<InvoiceEntity, Long>, JpaSpecificationExecutor<InvoiceEntity> {

    @Modifying
    @Query("update InvoiceEntity entity set entity.deleted = true where entity.id = :id")
    void softDeleteById(Long id);

    @Query("select entity from InvoiceEntity entity where entity.contractEntity.id in :contractIdList")
    List<InvoiceEntity> findByContractIdList(List<String> contractIdList);

    @Query("select entity from InvoiceEntity entity " +
            "where entity.contractEntity.id in :contractIdList " +
            "and entity.issueDate >= :start and entity.issueDate <=:end")
    List<InvoiceEntity> findByContractIdList(List<String> contractIdList, LocalDate start, LocalDate end);

    @Modifying
    @Query("update InvoiceEntity entity set entity.createdInPohoda = true where entity.id = :id")
    void updateCreatedInPohoda(Long id);

    @Query("select entity.id from InvoiceEntity entity where entity.contractEntity.id in :contractDtoSet")
    Set<Long> findIdByContractIdList(Set<String> contractDtoSet);

    @Query("select entity from InvoiceEntity entity where entity.dph.id = :id")
    List<InvoiceEntity> findListByVatId(String id);

    @Query("select entity.id from InvoiceEntity entity where entity.userEntity.id = :userId")
    Set<Long> getIdSetByOwner(Long userId);

    @Query("select entity from InvoiceEntity entity where entity.stringId = null")
    Set<InvoiceEntity> findInvoiceWithoutIdSet();
}
