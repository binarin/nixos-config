{ ... }:
{
  flake.nixosModules.kanata =
    { ... }:
    {
      key = "nixos-config.modules.nixos.kanata";

      services.kanata = {
        enable = true;
        keyboards.all.extraDefCfg = ''
          process-unmapped-keys yes
        '';
        keyboards.all.config = ''
          ;; Home row mods QWERTY example with more complexity.
          ;; Some of the changes from the basic example:
          ;; - when a home row mod activates tap, the home row mods are disabled
          ;;   while continuing to type rapidly
          ;; - tap-hold-release helps make the hold action more responsive
          ;; - pressing another key on the same half of the keyboard
          ;;   as the home row mod will activate an early tap action

          (defsrc
            esc  f1   f2   f3   f4   f5   f6   f7   f8   f9   f10  f11  f12        ssrq slck pause
            grv  1    2    3    4    5    6    7    8    9    0    -    =    bspc  ins  home pgup  nlck kp/  kp*  kp-
            tab  q    w    e    r    t    y    u    i    o    p    [    ]    \     del  end  pgdn  kp7  kp8  kp9  kp+
            caps a    s    d    f    g    h    j    k    l    ;    '    ret                        kp4  kp5  kp6
            lsft z    x    c    v    b    n    m    ,    .    /    rsft                 up         kp1  kp2  kp3  kprt
            lctl lmet lalt           spc            ralt rmet cmp  rctl            left down rght  kp0  kp.
          )


          (defvar
            ;; Note: consider using different time values for your different fingers.
            ;; For example, your pinkies might be slower to release keys and index
            ;; fingers faster.
            tap-time 200
            hold-time 150

            left-hand-keys (
              q w e r t
              a s d f g
              z x c v b
            )
            right-hand-keys (
              y u i o p
              h j k l ;
              n m , . /
            )
          )


          (deflayer base
            _    _    _    _    _    _    _    _    _    _    _    _    _          _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _     _    _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _    _    _     _    _    _     _    _    _    _
            @cps @a   @s   @d   @f   @g   @h   @j   @k   @l   @;   _    _                          _    _    _
            _    _    _    _    _    _    _    _    _    _    _    _                    _          _    _    _    _
            _    _    _             @spc            _    _    _    _               _    _    _     _    _
          )

          (deflayermap (nomods)
            a a s s d d f f g g h h j j k k l l ; ;
          )

          (deflayermap (fn)
            m menu
            i up

            h home
            k down
            j left
            l right
            ; end

            x del
            v ins
            e ret
            _ XX
          )


          (deflayermap (fn2)
            u mlft
            i @ma↑
            o mrgt
            p mmid
            h mbck
            j @ma←
            k @ma↓
            l @ma→
            ; mfwd
          )

          (deffakekeys
            to-base (layer-switch base)
          )

          (defalias
            ma↑ (movemouse-accel-up 10 1000 1 5)
            ma← (movemouse-accel-left 10 1000 1 5)
            ma↓ (movemouse-accel-down 10 1000 1 5)
            ma→ (movemouse-accel-right 10 1000 1 5)

            tap (multi
              (layer-switch nomods)
              ;; (on-idle-fakekey to-base tap 20)
            )

            fn (layer-toggle fn)
            fn2 (layer-toggle fn2)
            spc  (tap-hold-release $tap-time $hold-time spc @fn)
            cps (layer-toggle fn2) ;; (tap-hold-release $tap-time $hold-time esc @fn2)
            hpr (multi lsft lctl lalt lmet)

            a (tap-hold-release $tap-time $hold-time a lmet)
            s (tap-hold-release $tap-time $hold-time s lalt)
            d (tap-hold-release $tap-time $hold-time d lsft)
            f (tap-hold-release $tap-time $hold-time f lctl)
            g (tap-hold-press   $tap-time $hold-time g @hpr)
            h (tap-hold-press   $tap-time $hold-time h @hpr)
            j (tap-hold-release $tap-time $hold-time j rctl)
            k (tap-hold-release $tap-time $hold-time k rsft)
            l (tap-hold-release $tap-time $hold-time l ralt)
            ; (tap-hold-release $tap-time $hold-time ; rmet)
          )
        '';
      };
    };
}
