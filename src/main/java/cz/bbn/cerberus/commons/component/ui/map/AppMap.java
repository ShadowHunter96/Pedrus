package cz.bbn.cerberus.commons.component.ui.map;


import com.vaadin.addon.leaflet4vaadin.LeafletMap;
import com.vaadin.addon.leaflet4vaadin.layer.map.options.DefaultMapOptions;
import com.vaadin.addon.leaflet4vaadin.layer.map.options.MapOptions;
import com.vaadin.addon.leaflet4vaadin.layer.ui.marker.Marker;
import com.vaadin.addon.leaflet4vaadin.types.Icon;
import com.vaadin.addon.leaflet4vaadin.types.LatLng;
import com.vaadin.flow.component.orderedlayout.VerticalLayout;
import com.vaadin.flow.component.textfield.TextField;
import com.vaadin.flow.data.value.ValueChangeMode;
import cz.bbn.cerberus.commons.component.ui.autocomplete.Autocomplete;
import cz.bbn.cerberus.commons.enviromennt.AppEnv;
import cz.bbn.cerberus.commons.enviromennt.AppProperty;
import cz.bbn.cerberus.commons.geolocation.GeocodedLocation;
import cz.bbn.cerberus.commons.geolocation.GeocodingException;
import cz.bbn.cerberus.commons.geolocation.OpenStreetMapGeocoder;
import cz.bbn.cerberus.commons.notification.ErrorNotification;
import cz.bbn.cerberus.translation.Transl;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;


@Slf4j
public class AppMap extends VerticalLayout {

    private final TextField longitude;
    private final TextField latitude;
    private final AppEnv appEnv;

    private Collection<GeocodedLocation> geocodedLocationList;
    private LeafletMap leafletMap;

    private boolean locationEditable = true;


    public AppMap(TextField longitude, TextField latitude, AppEnv appEnv) {
        this.longitude = longitude;
        this.latitude = latitude;
        this.appEnv = appEnv;
        initComponent();
    }

    private void initComponent() {
        setPadding(false);
        setMargin(false);

        this.getStyle().set("overflow", "auto");

        OpenStreetMapGeocoder geocoder = OpenStreetMapGeocoder.getInstance();
        geocoder.setLimit(10);

        Autocomplete autocomplete = new Autocomplete(10, 700, Transl.get("Search address"));
        autocomplete.setCaseSensitive(false);
        autocomplete.addChangeListener(event -> {
            String text = event.getValue();
            List<String> stringList = new ArrayList<>();
            if (text.length() < 3) {
                autocomplete.setOptions(stringList);
                return;
            }
            try {
                geocodedLocationList = geocoder.geocode(text);
                geocodedLocationList.forEach(geocodedLocation -> stringList.add(geocodedLocation.getGeocodedAddress()));
            } catch (GeocodingException e) {
                ErrorNotification.show("Geolocation error", appEnv);
                log.error("Geolocation error", e);
            }
            autocomplete.setOptions(stringList);
        });

        autocomplete.addAutocompleteValueAppliedListener(event -> {
            GeocodedLocation actualGeocodedLocation = geocodedLocationList.stream()
                    .filter(geocodedLocation -> geocodedLocation.getGeocodedAddress()
                            .equalsIgnoreCase(event.getValue())).findFirst().orElse(null);
            if (actualGeocodedLocation != null) {
                longitude.setValue(String.valueOf(actualGeocodedLocation.getLon()));
                latitude.setValue(String.valueOf(actualGeocodedLocation.getLat()));
                initMap();
            }
        });

        this.add(autocomplete);

        longitude.addValueChangeListener(textFieldStringComponentValueChangeEvent -> initMap());
        longitude.setValueChangeMode(ValueChangeMode.TIMEOUT);
        longitude.setValueChangeTimeout(700);

        latitude.addValueChangeListener(textFieldStringComponentValueChangeEvent -> initMap());
        latitude.setValueChangeMode(ValueChangeMode.TIMEOUT);
        latitude.setValueChangeTimeout(700);

        this.setHeight("20em");
        this.setWidth("100%");
        this.setMaxWidth("25em");

        initMap();
    }

    public void initMap() {
        MapOptions options = new DefaultMapOptions();
        LatLng actualLocation = getLocation();
        options.setCenter(getLocation());
        options.setZoom(17);
        options.setMaxZoom(17);

        if (leafletMap != null) {
            this.remove(leafletMap);
        }
        leafletMap = new LeafletMap(options);
        leafletMap.setBaseUrl("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png");
        Marker marker = new Marker(actualLocation);
        marker.addTo(leafletMap);
        marker.setIcon(new Icon("https://unpkg.com/leaflet@1.9.2/dist/images/marker-icon.png"));
        leafletMap.setView(actualLocation, 17);
        leafletMap.setSizeFull();


        leafletMap.onClick(e -> {
            if (locationEditable) {
                longitude.setValue(String.valueOf(e.getLatLng().getLng()));
                latitude.setValue(String.valueOf(e.getLatLng().getLat()));
                initMap();
            }
        });

        this.add(leafletMap);
    }

    private LatLng getLocation() {
        if (StringUtils.isNoneEmpty(longitude.getValue()) && StringUtils.isNoneEmpty(latitude.getValue())) {
            return new LatLng(Double.parseDouble(latitude.getValue()), Double.parseDouble(longitude.getValue()));
        } else {
            double defaultLongitude = appEnv.getDoubleProperty(AppProperty.DEFAULT_LONGITUDE, 0);
            double defaultLatitude = appEnv.getDoubleProperty(AppProperty.DEFAULT_LATITUDE, 0);
            return new LatLng(defaultLatitude, defaultLongitude);
        }
    }


    public void setLocationEditable(boolean locationEditable) {
        this.locationEditable = locationEditable;
    }
}
