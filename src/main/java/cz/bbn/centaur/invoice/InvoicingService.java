package cz.bbn.cerberus.invoice;

import com.vaadin.flow.data.provider.Query;
import cz.bbn.cerberus.applog.AppLogService;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.areatechnologysign.factory.AreaTechnologySignFactory;
import cz.bbn.cerberus.areatechnologysign.persistance.AreaTechnologySignEntity;
import cz.bbn.cerberus.areatechnologysign.persistance.AreaTechnologySignRepository;
import cz.bbn.cerberus.commons.AppUtils;
import cz.bbn.cerberus.commons.DomainEnum;
import cz.bbn.cerberus.commons.component.ui.domain.ItemDto;
import cz.bbn.cerberus.commons.convertible.ConvertEntities;
import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.exception.ErrorCode;
import cz.bbn.cerberus.commons.exception.SystemException;
import cz.bbn.cerberus.commons.security.SecondaryOwnerProvider;
import cz.bbn.cerberus.commons.security.SecurityUtils;
import cz.bbn.cerberus.contract.ContractService;
import cz.bbn.cerberus.contract.dto.ContractDto;
import cz.bbn.cerberus.invoice.dto.InvoiceDto;
import cz.bbn.cerberus.invoice.dto.InvoiceFilterDto;
import cz.bbn.cerberus.invoice.factory.InvoiceFactory;
import cz.bbn.cerberus.invoice.persistance.entity.InvoiceEntity;
import cz.bbn.cerberus.invoice.persistance.repository.InvoiceDao;
import cz.bbn.cerberus.invoice.persistance.repository.InvoiceRepository;
import cz.bbn.cerberus.invoice.ui.InvoicingDetailView;
import cz.bbn.cerberus.listconfiguration.ListService;
import cz.bbn.cerberus.notification.NotificationService;
import cz.bbn.cerberus.permission.Permission;
import cz.bbn.cerberus.subject.persistance.SubjectEntity;
import cz.bbn.cerberus.subject.ui.SubjectDetailView;
import cz.bbn.cerberus.translation.Transl;
import cz.bbn.cerberus.user.dto.UserDto;
import cz.bbn.cerberus.user.persistance.UserEntity;
import cz.bbn.cerberus.usermessage.MessageType;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Sort;
import org.springframework.stereotype.Service;

import javax.transaction.Transactional;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;

@Service
public class InvoicingService extends SecondaryOwnerProvider {

    private final InvoiceRepository invoiceRepository;
    private final ContractService contractService;
    private final ListService listService;
    private final AppLogService appLogService;
    private final InvoiceDao invoiceDao;
    private final AreaTechnologySignRepository areaTechnologySignRepository;
    private final NotificationService notificationService;
    private final AppEnv appEnv;

    public InvoicingService(InvoiceRepository invoiceRepository, InvoiceDao invoiceDao, ContractService contractService,
                            ListService listService, AppLogService appLogService,
                            AreaTechnologySignRepository areaTechnologySignRepository, NotificationService notificationService, AppEnv appEnv) {
        this.invoiceRepository = invoiceRepository;
        this.invoiceDao = invoiceDao;
        this.contractService = contractService;
        this.listService = listService;
        this.appLogService = appLogService;
        this.areaTechnologySignRepository = areaTechnologySignRepository;
        this.notificationService = notificationService;
        this.appEnv = appEnv;
    }

    public Page<InvoiceDto> getInvoiceDtoByContractPage(
            List<ContractDto> contractDtoList, Query<InvoiceDto, Void> query, List<Sort.Order> orderList) {
        return invoiceDao.getInvoiceDtoByContractPage(contractDtoList, query, orderList);
    }

    public Page<InvoiceDto> getInvoiceDtoPage(InvoiceFilterDto invoiceFilterDto) {
        return invoiceDao.getInvoiceDtoPage(invoiceFilterDto);
    }

    public Long getInvoiceCountForDashboard(LocalDate from, LocalDate to) {
        Set<String> objectIdSet = SecurityUtils.getAllowedEntityIdByDomain(
                Permission.CONTRACT_EDIT.name(), DomainEnum.CONTRACT_DOMAIN_NAME.getValue());
        return listService.getInvoiceDtoList().stream().filter(invoiceDto -> {
            LocalDate date = invoiceDto.getInvoicingDate();
            return date != null && (date.isAfter(from) || date.isEqual(from))
                    && (date.isBefore(to) || date.isEqual(to))
                    && objectIdSet.contains(invoiceDto.getContractDto().getId());
        }).count();
    }

    public Map<ItemDto, Double> getInvoicesMapByUser(List<UserDto> userDtoList, LocalDate from, LocalDate to) {
        Map<ItemDto, Double> actualMap = AppUtils.getUserMapWithDouble(userDtoList);
        listService.getInvoiceDtoList().stream().forEach(invoiceDto -> {
            LocalDate date = invoiceDto.getInvoicingDate();
            if (!Boolean.TRUE.equals(invoiceDto.getDeleted()) && date != null
                    && (date.isAfter(from) || date.isEqual(from)) && (date.isBefore(to) || date.isEqual(to))
                    && invoiceDto.getUserDto() != null) {
                UserDto userDto = invoiceDto.getUserDto();
                Double count = actualMap.get(new ItemDto(userDto)) + 1;
                actualMap.put(new ItemDto(userDto), count);
            }
        });
        return AppUtils.removeZeroAndGetMap(actualMap);
    }

    @Transactional
    public void softDelete(String id) throws SystemException {
        if (existById(Long.parseLong(id))) {
            invoiceRepository.softDeleteById(Long.parseLong(id));
            appLogService.logDelete(id, DomainEnum.INVOICE_DOMAIN_NAME.getValue());
        } else {
            throw new SystemException(ErrorCode.INVOICE_DOES_NOT_EXIST, id);
        }
    }

    private boolean existById(Long id) {
        if (id != null) {
            return invoiceRepository.existsById(id);
        }
        return false;
    }

    @Transactional
    public Long saveAll(List<InvoiceDto> invoiceDtoList, Set<AreaTechnologySignDto> areaTechnologySignDtoSet) {
        List<InvoiceEntity> entityList = new ArrayList<>();
        for (InvoiceDto dto : invoiceDtoList) {
            InvoiceEntity entity = new InvoiceEntity();
            InvoiceFactory.fillEntity(entity, dto);
            entityList.add(entity);
            appLogService.logInsert(dto, DomainEnum.INVOICE_DOMAIN_NAME.getValue());
        }
        List<InvoiceEntity> invoiceEntityList = invoiceRepository.saveAll(entityList);
        List<AreaTechnologySignEntity> areaTechnologySignEntityList = new ArrayList<>();
        invoiceEntityList.forEach(invoiceEntity -> areaTechnologySignDtoSet.forEach(areaTechnologySignDto -> {
            AreaTechnologySignEntity areaTechnologySignEntity = new AreaTechnologySignEntity();
            AreaTechnologySignFactory.toEntity(areaTechnologySignEntity, areaTechnologySignDto);
            areaTechnologySignEntity.setObjectType(ObjectType.INVOICE);
            areaTechnologySignEntity.setObjectId(String.valueOf(invoiceEntity.getId()));
            areaTechnologySignEntityList.add(areaTechnologySignEntity);
        }));
        invoiceEntityList.forEach(this::generateNotificationEntity);
        areaTechnologySignRepository.saveAll(areaTechnologySignEntityList);
        listService.reloadInvoiceList();
        updateInvoiceWithoutId();

        return invoiceEntityList.get(invoiceEntityList.size() - 1).getId();
    }

    public InvoiceDto findById(Long id) throws SystemException {
        InvoiceDto invoiceDto = InvoiceFactory.fromEntity(invoiceRepository.findById(id).orElseThrow(
                () -> new SystemException(ErrorCode.INVOICE_DOES_NOT_EXIST, String.valueOf(id))));
        List<AreaTechnologySignEntity> areaTechnologySignEntityList =
                areaTechnologySignRepository.findByObjectTypeAndObjectId(
                        ObjectType.INVOICE, String.valueOf(invoiceDto.getId()));
        invoiceDto.setAreaTechnologySignDtoSet(new HashSet<>(
                ConvertEntities.fromEntities(areaTechnologySignEntityList, AreaTechnologySignFactory::fromEntity)));
        return invoiceDto;
    }

    @Transactional
    public void saveInvoiceDto(InvoiceDto invoiceDto) throws SystemException {
        if (!existById(invoiceDto.getId())) {
            InvoiceEntity entity = new InvoiceEntity();
            InvoiceFactory.fillEntity(entity, invoiceDto);
            InvoiceEntity invoiceEntity = invoiceRepository.save(entity);
            invoiceDto.setId(invoiceEntity.getId());
            deleteAndSaveAreaTechnologySign(invoiceDto);
            generateNotificationEntity(invoiceEntity);
            listService.reloadInvoiceList();
            appLogService.logInsert(invoiceDto, DomainEnum.INVOICE_DOMAIN_NAME.getValue());
            updateInvoiceWithoutId();
        } else {
            throw new SystemException(ErrorCode.INVOICE_ALREADY_EXISTS, invoiceDto.getId());
        }
    }

    @Transactional
    public void updateInvoiceDto(InvoiceDto dto, InvoiceDto originalDto) throws SystemException {
        if (existById(dto.getId())) {
            InvoiceEntity entity = new InvoiceEntity();
            InvoiceFactory.fillEntity(entity, dto);
            invoiceRepository.save(entity);
            deleteAndSaveAreaTechnologySign(dto);
            listService.reloadInvoiceList();
            appLogService.logUpdate(dto, originalDto, DomainEnum.INVOICE_DOMAIN_NAME.getValue());
        } else {
            throw new SystemException(ErrorCode.INVOICE_DOES_NOT_EXIST, dto.getId());
        }
    }

    private void deleteAndSaveAreaTechnologySign(InvoiceDto dto) {
        if (dto.getAreaTechnologySignDtoSet() == null) {
            return;
        }
        areaTechnologySignRepository.deleteByObjectTypeAndObjectId(ObjectType.INVOICE, String.valueOf(dto.getId()));
        List<AreaTechnologySignEntity> areaTechnologySignEntityList = new ArrayList<>();
        dto.getAreaTechnologySignDtoSet().forEach(areaTechnologySignDto -> {
            AreaTechnologySignEntity areaTechnologySignEntity = new AreaTechnologySignEntity();
            AreaTechnologySignFactory.toEntity(areaTechnologySignEntity, areaTechnologySignDto);
            areaTechnologySignEntity.setObjectType(ObjectType.INVOICE);
            areaTechnologySignEntity.setObjectId(String.valueOf(dto.getId()));
            areaTechnologySignEntityList.add(areaTechnologySignEntity);
        });
        areaTechnologySignRepository.saveAll(areaTechnologySignEntityList);
    }

    @Transactional
    public void updateCreatedInPohoda(Long id) {
        invoiceRepository.updateCreatedInPohoda(id);
    }

    public List<InvoiceDto> findInvoiceDtoByAllowedContractList(LocalDate start, LocalDate end) {
        List<ContractDto> contractDtoList = contractService.getMyInvoiceEditContractList();
        List<String> contractIdList = new ArrayList<>();
        for (ContractDto contractDto : contractDtoList) {
            contractIdList.add(contractDto.getId());
        }
        return ConvertEntities.fromEntities(
                invoiceRepository.findByContractIdList(contractIdList, start, end), InvoiceFactory::fromEntity);
    }

    public List<InvoiceDto> getInvoiceByVatId(String id) {
        return ConvertEntities.fromEntities(invoiceRepository.findListByVatId(id), InvoiceFactory::fromEntity);
    }

    public void generateNotificationEntity(InvoiceEntity invoiceEntity) {
        SubjectEntity subjectEntity = invoiceEntity.getContractEntity().getSubject();
        UserEntity userEntity = subjectEntity.getUserEntity();
        String language = Optional.ofNullable(userEntity.getPreferredLanguage()).orElse(Transl.DEFAULT_LANG);

        if (!userEntity.getId().equals(SecurityUtils.getCurrentUserId())) {
            String message = Transl.getByLang("User", language).concat(" ").concat(SecurityUtils.getCurrentUserName()).concat(" ")
                    .concat(Transl.getByLang("created a new invoice width date", language).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), InvoicingDetailView.ROUTE,
                                    String.valueOf(invoiceEntity.getId()), AppUtils.formatDate(invoiceEntity.getInvoicingDate()))).concat(" ")
                            .concat(Transl.getByLang("on subject", language)).concat(" ")
                            .concat(AppUtils.generateUrl(appEnv.getProjectUrl(), SubjectDetailView.ROUTE, subjectEntity.getId(), subjectEntity.getName())));
            notificationService.saveEmailLowNotification(MessageType.NEW_INVOICE.name(), message, userEntity.getId());
        }
    }

    private void updateInvoiceWithoutId() {
        Set<InvoiceEntity> invoiceSet = invoiceRepository.findInvoiceWithoutIdSet();
        for (InvoiceEntity invoiceEntity : invoiceSet) {
            invoiceEntity.setStringId(AppUtils.generateId("FO", invoiceEntity.getId().intValue()));
        }
    }

    @Override
    protected Set<String> getUserEntityIdSet(Long userId) {
        Set<String> stringIdSet = new HashSet<>();
        Set<Long> longIdSet = invoiceRepository.getIdSetByOwner(userId);
        for (Long longId : longIdSet) {
            stringIdSet.add(String.valueOf(longId));
        }
        return stringIdSet;
    }

    @Override
    protected Set<DomainEnum> getDomainSet() {
        return EnumSet.of(DomainEnum.INVOICE_DOMAIN_NAME);
    }
}
