package cz.bbn.cerberus.suppliertype;

import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.suppliertype.dto.SupplierTypeDto;
import cz.bbn.cerberus.suppliertype.factory.SupplierTypeFactory;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeEntity;
import cz.bbn.cerberus.suppliertype.persistance.SupplierTypeRepository;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.util.List;

@Service
public class SupplierTypeService {

    private final SupplierTypeRepository supplierTypeRepository;
    private final ListService listService;
    private final AppLogService appLogService;

    public SupplierTypeService(SupplierTypeRepository supplierTypeRepository, AppLogService appLogService,
                               ListService listService) {
        this.supplierTypeRepository = supplierTypeRepository;
        this.appLogService = appLogService;
        this.listService = listService;
    }

    public Page<SupplierTypeDto> findSupplierTypeDtoPage(int page, int size, List<Sort.Order> sortList) {
        Page<SupplierTypeEntity> pageEntity = supplierTypeRepository.findAll(
                PageRequest.of(page, size, Sort.by(sortList)));
        List<SupplierTypeDto> list = ConvertEntities
                .fromEntities(pageEntity.toList(), SupplierTypeFactory::fromEntity);
        return new PageImpl<>(list, PageRequest.of(page,
                size, Sort.by(sortList)),
                pageEntity.getTotalElements());
    }

    public List<SupplierTypeDto> findAllAllowedExceptOne(String exceptId) {
        List<SupplierTypeEntity> entityList = supplierTypeRepository.findAllAllowedExceptOne(exceptId);
        return ConvertEntities
                .fromEntities(entityList, SupplierTypeFactory::fromEntity);
    }

    public List<SupplierTypeDto> findAll() {
        List<SupplierTypeEntity> entityList = supplierTypeRepository.findAll();
        return ConvertEntities
                .fromEntities(entityList, SupplierTypeFactory::fromEntity);
    }

    public boolean isSupplierTypeAllowed(String id) {
        return supplierTypeRepository.isContactPersonTypeAllowed(id);
    }

    public SupplierTypeDto getSupplierType(String id) throws SystemException {
        SupplierTypeEntity entity = getSupplierTypeEntity(id);
        return SupplierTypeFactory.fromEntity(entity);
    }

    public boolean supplierTypeExists(String id) {
        return supplierTypeRepository.existsById(id);
    }

    @Transactional
    public void saveSupplierType(SupplierTypeDto dto) throws SystemException {
        if (supplierTypeExists(dto.getId())) {
            throw new SystemException(ErrorCode.ID_ALREADY_EXISTS, dto.getId());
        }
        SupplierTypeEntity entity = new SupplierTypeEntity();
        saveSupplierType(entity, dto);
        appLogService.logInsert(dto, DomainEnum.SUPPLIER_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void updateSupplierType(SupplierTypeDto dto, SupplierTypeDto originalDto) throws SystemException {
        SupplierTypeEntity entity = getSupplierTypeEntity(dto.getId());
        this.saveSupplierType(entity, dto);
        appLogService.logUpdate(dto, originalDto, DomainEnum.SUPPLIER_TYPE_DOMAIN_NAME.getValue());
    }

    @Transactional
    public void deleteSupplierType(String id) throws SystemException {
        if (!supplierTypeRepository.existsById(id)) {
            throw new SystemException(ErrorCode.SUPPLIER_TYPE_NOT_EXISTS, id);
        }
        supplierTypeRepository.deleteById(id);
        listService.reloadSupplierTypeDtoList();
        appLogService.logDelete(id, DomainEnum.SUPPLIER_TYPE_DOMAIN_NAME.getValue());
    }

    private void saveSupplierType(SupplierTypeEntity entity, SupplierTypeDto dto) {
        SupplierTypeFactory.fillEntity(entity, dto);
        supplierTypeRepository.save(entity);
        listService.reloadSupplierTypeDtoList();
    }

    private SupplierTypeEntity getSupplierTypeEntity(String id) throws SystemException {
        return supplierTypeRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.SUPPLIER_TYPE_NOT_EXISTS, id));
    }

    public List<SupplierTypeDto> getAllowedSupplierTypes() {
        return ConvertEntities.fromEntities(supplierTypeRepository.findAllAllowed(), SupplierTypeFactory::fromEntity);
    }
}
