package cz.bbn.cerberus.offer.dto;

import cz.bbn.cerberus.commons.enums.ObjectType;
import cz.bbn.cerberus.subject.dto.SubjectDto;
import lombok.Getter;
import lombok.Setter;
import org.springframework.data.domain.Sort;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.Set;


@Getter
@Setter
public class OfferFilterDto {

    private String nameOrId;
    private Set<OfferState> offerStateSet;
    private Set<Boolean> sent;
    private Set<SubjectDto> customerSet;
    private BigDecimal priceFrom;
    private BigDecimal priceTo;

    private LocalDate validityDateFrom;
    private LocalDate validityDateTo;

    private String objectId;
    private ObjectType objectType;

    private boolean showDeleted;
    private boolean onlyEditPermission;

    private int page;
    private int size;
    private List<Sort.Order> orderList;
}
