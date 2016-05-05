# tclres-config.nix
{
  network.description = "TCLaa Reservation System";

  tclres =
    { config, pkgs, ... }:
    let
      #tclres = import ./default.nix {};
      tclres = import /home/odi/src/tclaa/reservation/default.nix {};
    in
    {
      networking.hostName = "tclres";
      networking.firewall.allowedTCPPorts = [ 22 80 8000 ];

      environment.systemPackages = [ pkgs.sqlite tclres ];

      systemd.services.tclres = {
        description = "TCLaa Reservation System";
	wantedBy = [ "multi-user.target" ];
	after = [ "network.target" ];
	serviceConfig = {
	  WorkingDirectory = "/www";
	  ExecStart = "${tclres}/bin/reservation ${tclres}/ /www/reservation.db '/tclaa-test' --error-log='/var/log/tclres-error.log' --access-log='/var/log/tclres-access.log'";
	};
      };
    };
}