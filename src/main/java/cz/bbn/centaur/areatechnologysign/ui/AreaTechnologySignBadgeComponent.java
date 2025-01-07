package cz.bbn.cerberus.areatechnologysign.ui;

import com.vaadin.flow.component.icon.Icon;
import com.vaadin.flow.component.icon.VaadinIcon;
import com.vaadin.flow.component.orderedlayout.HorizontalLayout;
import cz.bbn.cerberus.area.dto.AreaDto;
import cz.bbn.cerberus.areatechnologysign.AreaTechnologySignDto;
import cz.bbn.cerberus.commons.component.ui.VaadinComponents;
import cz.bbn.cerberus.commons.component.ui.interfaces.DeleteActionByDto;
import cz.bbn.cerberus.technology.dto.TechnologyDto;
import org.vaadin.addons.badge.Badge;

public class AreaTechnologySignBadgeComponent extends HorizontalLayout {

    private final AreaTechnologySignDto areaTechnologySignDto;
    private final DeleteActionByDto<AreaTechnologySignDto> deleteActionByDto;

    public AreaTechnologySignBadgeComponent(AreaTechnologySignDto areaTechnologySignDto, DeleteActionByDto<AreaTechnologySignDto> deleteActionByDto) {
        this.areaTechnologySignDto = areaTechnologySignDto;
        this.deleteActionByDto = deleteActionByDto;
        initComponent();
    }

    private void initComponent(){
        this.getElement().getStyle().set("margin-top", "1em");
        this.getElement().getStyle().set("margin-right", "1em");
        AreaDto areaDto = areaTechnologySignDto.getAreaDto();
        TechnologyDto technologyDto = areaTechnologySignDto.getTechnologyDto();

        if(areaDto != null){
            this.add(getBadge(areaDto.getName(), areaDto.getBadgeVariant(), areaDto.getIcon()));
        }

        if(technologyDto != null){
            Badge technologyBadge = getBadge(technologyDto.getName(), technologyDto.getBadgeVariant(), technologyDto.getIcon());
            this.add(technologyBadge);
            if(areaDto != null){
                technologyBadge.getElement().getStyle().set("margin-left", "-1em");
            }
        }
        if(deleteActionByDto != null) {
            Icon delete = VaadinIcon.CLOSE.create();
            delete.setSize("10px");
            delete.addClassName("close-cross");
            delete.addClassName("badge-delete");
            delete.addClickListener(buttonClickEvent ->
                    deleteActionByDto.deleteItem(areaTechnologySignDto));
            this.add(delete);
        }
    }

    public Badge getBadge(String name, Badge.BadgeVariant badgeVariant, VaadinIcon vaadinIcon) {
        return VaadinComponents.getBadge(name,
                badgeVariant,
                vaadinIcon);
    }
}
